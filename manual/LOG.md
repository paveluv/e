# Echo area and structured log

## Echo area

The echo area shows the transient tail of the editor log. Each new entry is a
separate row prefixed with its component, so one command can report several
events without overwriting earlier output. The area grows by shrinking windows
to their configured minimum height. When it fills, older visible entries give
way, but remain in the log.

The next keyboard event, mouse click, or wheel event settles the echo area back
to its live line.

Messages whose `message-source` is `#f` are temporary indicators. They appear
in the echo area but are not recorded. Prompts and modes use indicators for
state that is useful now but not historical.

## Progress entries

Code may mark a message as progress:

```scheme
(parameterize ([message-progress #t])
  (log! 'download "Receiving page 4"))
```

A progress entry supersedes the newest visible entry from the same component
instead of stacking. It never replaces another component's entry. Every
progress update still receives its own record in `*log*`.

## The `*log*` view

`*log*` is a dynamic, read-only view backed by structured records. It is always
present in the buffer list. At the end of the buffer it tails new entries;
elsewhere its viewport remains still while records arrive.

Filtered log views are created dynamically:

```scheme
(log-view 'eval)
```

This creates a buffer such as `*log eval*` containing only that component.

Each record contains a nanosecond timestamp, component, and value. Components
may register both a formatter and a styler, shared by the echo area and log
views. Eval, for example, stores `(query . result)` and presents it as
`query => result` with Scheme styling.

## History

`log-history` derives command histories from structured component records.
File prompts use it to recall visited and saved paths; `M-x` uses eval records
for expression history. History is therefore presentation-independent and
does not scrape rendered text.

## API

- `log!` adds a structured record.
- `log-entries` returns records for filtering or inspection.
- `log-length` reports the record count.
- `log-history` derives values for interactive history.
- `register-log-formatter!` installs component presentation.
- `present-log-entry!` and `present-log-entries!` expose the shared echo
  presentation path.
- `set-message!` records or displays according to `message-source`.
- `show-message!` displays an explicit transient message and styles.

Errors that indicate an editor or extension failure are reported in both the
echo area and log instead of being swallowed.

