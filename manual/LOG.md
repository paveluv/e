# Echo area and structured log

## Echo area

The echo area shows this head's messages from the shared editor log. Each new entry is a
separate row prefixed with its component, so one command can report several
events without overwriting earlier output. The area grows by shrinking windows
to their configured minimum height. When it fills, older visible entries give
way, but remain in the log until its retention limit expires them. Other
actors' records are available in `<log>` without interrupting this head's
echo area.

The next keyboard event, mouse click, or wheel event settles the echo area back
to its live line.

Messages whose `edit:message-source` is `#f` are temporary indicators. They appear
in the echo area but are not recorded. Prompts and modes use indicators for
state that is useful now but not historical.

## Progress entries

Code may mark a message as progress:

```scheme
(parameterize ([edit:message-progress #t])
  (log:add! 'download "Receiving page 4"))
```

A progress entry supersedes the newest visible entry from the same component
instead of stacking. It never replaces another component's entry. Every
progress update still receives its own record in `<log>`.
`edit:message-progress` is the command layer's alias for `log:progress`. Its value
is local to the calling thread and captured when the record is appended, so
concurrent or deferred delivery preserves the requested presentation.

## The `<log>` view

`<log>` is a dynamic, read-only view backed by structured records. It is always
present in the buffer list. At the end of the buffer it tails new entries;
elsewhere its viewport stays with the same text while records arrive. The base
retains the newest **1,000,000 records** across all components by default.
Log views render the most recent **4,096 records**, filtering within that
window, so opening a head does not fetch or format the entire journal. Older
retained records remain available to component queries and command histories.
Views drop rows outside this window or the journal's retention on refresh;
points, marks and viewports in surviving text move with it,
and positions in expired text move to the start. Hidden views catch up when
shown again. A multiline record expires as a whole.

This is recent, in-memory history for the lifetime of the base. Restarting
the base clears it. The limit counts records, not payload bytes or rendered
lines; an individual record or formatter result can still be large.

Set the shared retention in `base-config.e`:

```scheme
(log:retention 1000000)
```

`(log:retention)` returns the current limit. The setter accepts a positive
exact integer that fits a vector size and returns the new limit. It takes
effect immediately in the base; views observe it at their next refresh.
Shrinking discards the oldest excess records. Growing preserves what remains
and never restores evicted records. Append bookmarks remain valid across
resizing. This is one shared setting, so an authorized attached head can also
change it through M-x; put it in `base-config.e` to keep it across restarts.

Filtered log views are created dynamically:

```scheme
(log-view:buffer! 'eval)
```

This creates a buffer such as `<log eval>` containing only that component.

Each row shows a timestamp, actor identity, component, and value. The base
keeps one history; each head's `<log>` and filtered views are local renderings.
Components
may register both a formatter and a styler, shared by the echo area and log
views. Eval, for example, stores `(query . result)` and presents it as
`query => result` with Scheme styling.

Stored records have the plain shape `(utc-nanoseconds actor component datum)`.
The timestamp is an exact integer since the Unix epoch. `log:time`,
`log:actor`, `log:component`, and `log:datum` select the fields. Attribution
comes from `actor:current`, or `(base e)` outside an actor context; logging
does not require directory registration. The actor identifies the work that
logged the message, which may describe another actor's operation.

Finite plain data stay structured. A runtime object or cyclic datum is
captured as its written representation at logging time. Inputs, returned
records, reads and subscriber deliveries do not share mutable data with the
stored history. Formatting or changing a returned value cannot rewrite an
earlier record.

## History

`log:history` derives command histories from structured component records.
File prompts use it to recall visited and saved paths; `M-x` uses eval records
for expression history. History is therefore presentation-independent and
does not scrape rendered text. Each history read considers the newest 200
retained records of that component and, when supplied, actor. It selects
strings and collapses consecutive repeats. Other components and excluded
actors do not consume that read allowance, but share the base's overall
retention limit. Find-file uses this head's visits and removes older repeats
so each path appears once; successful revisits become recent again.

Policy activity uses the same structured log, with no separate audit history:

```scheme
(map log:datum (log:entries 'policy))
```

These events are recorded quietly and remain visible in `<log policy>`.

The base records shared store operations once, including while all heads are
absent. `<log store>` shows the operation's actor and compact data:
`(create id name)`, `(rename id name)`, `(delete id)`, `(property id key)`,
`(reset id revision)`, or `(edit id revision span [history-origin])`.
Edit records omit text payloads; undo/redo keep their existing origin data.
These records are quiet. Heads also contribute `ui: …` summaries with the
revision range of a typing burst, and local resync diagnostics. Their time
of presentation is separate from the base's operation order.

## API

- `(log:add! component datum [show?])` adds a record; the component is a
  symbol, and `show?` defaults to true. Passing `#f` logs quietly.
- `(log:retention [count])` reads or changes the shared retention limit.
  Attached setters require an all-buffer human head; readers may query it.
- `(log:entries [component [count]])` returns owned records, newest first.
  `component` is a symbol or `#f` for all components. `count` is a nonnegative
  exact integer or `#f` for all retained matches. Filtering and limiting happen
  before copying records.
- `(log:snapshot [start [count [component [actor]]]])` returns three values: owned
  records, the captured end bookmark, and the oldest retained index. `start`
  defaults to zero; count/component have the same meanings as above. An
  optional actor identity filters before limiting and copying; `#f` includes
  every actor. Indexes
  are absolute append positions within this base's lifetime. A start older
  than the retention floor clamps to that floor; a future or invalid start
  refuses. The floor is global even for a filtered read.
- For incremental catch-up, omit the count limit and use the returned end as
  the next start, even when there are no matching records. If `start` is below
  the returned floor, older records have expired: rebuild from the retained
  snapshot or discard those old rows. A count-limited read intentionally
  returns only the newest matches, while its end still covers the whole read.
  `(log:snapshot 0 0)` reads just the bounds without copying payloads.
  Formatting callbacks run after the snapshot; additions belong to the next read.
  On attached connections, use bounded tail/component reads: a full journal
  snapshot can exceed the wire's 16 MiB frame limit. The log view does not
  page through older history.
- `(log:history component [selector [actor]])` derives strings for interactive
  history. The selector receives each record's datum and defaults to identity.
- `log:register-formatter!` installs component presentation.
- `edit:present-log-entry!` and `edit:present-log-entries!` expose the shared echo
  presentation path.
- `edit:set-message!` records or displays according to `edit:message-source`.
- `paint:show-message!` displays an explicit transient message and styles.

`(log:subscribe! procedure)` returns a token for `log:unsubscribe!`. The
procedure receives `(entry presentation)`, where presentation is `#f`,
`append`, or `progress`. It hears quiet records too. Subscribers belong to
the module registering them and are retracted on reload; newly registered
subscribers hear subsequent appends, and revocation cancels queued delivery.

Callbacks run outside the log's writer lock, in append order, under the
originating actor and progress context. Concurrent and reentrant appends
commit without waiting for an active callback. A failing subscriber cannot
remove the record or stop another subscriber. Read a snapshot to catch up
on history rather than expecting a new subscription to replay it.
Eviction does not cancel an already queued subscription delivery. These
retention bounds do not cap callback backlogs or snapshots held by readers.

Errors that indicate an editor or extension failure are reported in both the
echo area and log instead of being swallowed.
