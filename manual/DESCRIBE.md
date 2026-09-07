# Describe and reference data

## Interactive commands

`C-h f` runs `describe:show!!`, prompting for a documented Scheme name with
completion. `M-.` describes the symbol at point in a Scheme buffer. `C-h k`
describes a key, its resolved command, origin, shadowed bindings, and contextual
meanings.

Describe opens a read-only `<describe>` view in a new tile below the current
window, or reuses a window already showing it. Focus remains in the requesting
window. The base keeps one read-only Markdown source, `*describe*`, visible
to the requesting head; `<describe>` is its local rendered companion.
The selected name survives module reload, and pages update dynamically.
If the described value is a command, the page lists its current global keys;
rebinding or unbinding the command updates an already
visible page on redraw. `C-h k` uses the same behavior for `<help>`.

`C-c v` in the view shows its Markdown source, which remains read-only.
Killing the view keeps the source; describing another name reuses it.
Killing the source also closes its view. Renaming either buffer preserves
their relationship. A name with no documentation leaves the previous page
alone; if a displayed entry is later removed, the page reports its absence
and resumes displaying it when the documentation returns.

The completion prompt uses shared semantic styling: a partial name is italic,
a complete Scheme name is upright, and an e-specific name uses the editor
face.

## Reference corpus

The optional corpus combines R6RS documentation from TSPL4 with Chez Scheme
extensions from the Chez Scheme User's Guide. Fetch it once from inside e:

```scheme
(describe:fetch-data!)
```

The command downloads and extracts roughly 1,400 entries into
`data/describe/`, which is intentionally outside version control. It uses the
[HTTP client](HTTPS.md). Fetch steps are recorded in `<log>`; completion also
appears in the echo area. Queries remain available during a fetch, and another
fetch is refused until the current one finishes.

Without the downloaded corpus, module-published documentation remains
available.

## Structured queries

`describe:lookup` returns entries for a name. `describe:entries` returns the complete
collection, optionally filtered by a predicate. The entry accessors
(`doc:names`, `doc:forms`, `doc:returns`, `doc:libraries`, `doc:source`,
`doc:chapter`, `doc:url`, `doc:description`) expose each field, allowing the
manual to be queried by ordinary Scheme code.

```scheme
(describe:this eq-hashtable-ref)
(describe:lookup 'lambda)
```

Code that needs documentation data can import `(reference)` under `reference:`.
Its `lookup`, `entries`, and `browser-url` operations are also exported under
`describe:`; `reference:fetch!` is the same operation as `describe:fetch-data!`.
These queries include both the downloaded corpus and current `doc:` registrations.

For a head integration, `reference:page!` takes a head actor, a name and a list
of key annotations, returning the private source's store id or `#f` when the
name has no documentation. `reference:page` returns `(id revision name)` for
that head's current page, or `#f`. Passing `(id . revision)` as the fourth
argument to `page!` refreshes an existing selection only if it is still
current and retains its original private audience.

## Publishing module documentation

Modules add entries with `doc:register!`. Each entry has this shape:

```scheme
(names forms returns libraries source chapter url description)
```

Registrations belong to the calling module. Reloading retracts its previous
entries and installs the new collection transactionally, just like modes and
key bindings. A URL may be `#f`.

The `(edit)` module uses this mechanism for commands such as `replace!!` and
`replace-all!`. Registered forms also drive the grey parameter suggestion in
`M-x`, so newly documented procedures receive prompt hints automatically.
