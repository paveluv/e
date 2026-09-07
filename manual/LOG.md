# Echo area and structured log

## Echo area

The echo area shows this head's messages from the shared editor log. Each new entry is a
separate row prefixed with its component, so one command can report several
events without overwriting earlier output. The area grows by shrinking windows
to their configured minimum height. When it fills, older visible entries give
way, but remain in the log. Other actors' records remain available in `<log>`
without interrupting this head's echo area.

The next keyboard event, mouse click, or wheel event settles the echo area back
to its live line.

Messages whose `message-source` is `#f` are temporary indicators. They appear
in the echo area but are not recorded. Prompts and modes use indicators for
state that is useful now but not historical.

## Progress entries

Code may mark a message as progress:

```scheme
(parameterize ([message-progress #t])
  (log:add! 'download "Receiving page 4"))
```

A progress entry supersedes the newest visible entry from the same component
instead of stacking. It never replaces another component's entry. Every
progress update still receives its own record in `<log>`.
`message-progress` is the command layer's alias for `log:progress`. Its value
is local to the calling thread and captured when the record is appended, so
concurrent or deferred delivery preserves the requested presentation.

## The `<log>` view

`<log>` is a dynamic, read-only view backed by structured records. It is always
present in the buffer list. At the end of the buffer it tails new entries;
elsewhere its viewport remains still while records arrive.

Filtered log views are created dynamically:

```scheme
(log-view:buffer 'eval)
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
does not scrape rendered text.

## API

- `(log:add! component datum [show?])` adds a record; the component is a
  symbol, and `show?` defaults to true. Passing `#f` logs quietly.
- `(log:record index)` returns one owned record by its zero-based append
  index; an index outside the log raises an error.
- `(log:entries [component])` returns owned records, newest first, optionally
  filtered by component.
- `(log:snapshot [start])` returns two values: records from `start` (default
  zero), newest first, and the count captured with that snapshot. Use the
  returned count as the next start for an incremental reader. Formatting
  callbacks run after the snapshot, so additions belong to the next read.
- `log:length` reports the record count.
- `log:history` derives values for interactive history.
- `log:register-formatter!` installs component presentation.
- `present-log-entry!` and `present-log-entries!` expose the shared echo
  presentation path.
- `set-message!` records or displays according to `message-source`.
- `paint:show-message!` displays an explicit transient message and styles.

`(log:subscribe! procedure)` returns a token for `log:unsubscribe!`. The
procedure receives `(entry presentation)`, where presentation is `#f`,
`append`, or `progress`. It hears quiet records too. Subscribers belong to
the module registering them and are retracted on reload; newly registered
subscribers hear subsequent appends, and revocation cancels queued delivery.
This replaces the former single `log:set-presenter!` hook.

Callbacks run outside the log's writer lock, in append order, under the
originating actor and progress context. Concurrent and reentrant appends
commit without waiting for an active callback. A failing subscriber cannot
remove the record or stop another subscriber. Read a snapshot to catch up
on history rather than expecting a new subscription to replay it.

Errors that indicate an editor or extension failure are reported in both the
echo area and log instead of being swallowed.
