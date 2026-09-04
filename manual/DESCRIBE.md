# Describe and reference data

## Interactive commands

`C-h f` runs `describe!!`, prompting for a documented Scheme name with
completion. `M-.` describes the symbol at point in a Scheme buffer. `C-h k`
describes a key, its resolved command, origin, shadowed bindings, and contextual
meanings.

Describe opens a read-only `*describe*` view in a new tile below the current
window, or reuses a window already showing it. Focus remains in the requesting
window. Its data remains structured until rendering, so pages can update
dynamically. If the described value is a command, the page lists every key
currently bound to it; rebinding or unbinding the command updates an already
visible page on redraw. `C-h k` uses the same behavior for `*help*`.

The completion prompt uses shared semantic styling: a partial name is italic,
a complete Scheme name is upright, and an e-specific name uses the editor
face.

## Reference corpus

The optional corpus combines R6RS documentation from TSPL4 with Chez Scheme
extensions from the Chez Scheme User's Guide. Fetch it once from inside e:

```scheme
(fetch-describe-data!)
```

The command downloads and extracts roughly 1,400 entries into
`data/describe/`, which is intentionally outside version control. Downloading
uses `curl`; parsing and indexing are handled by `describe.e`.

Without the downloaded corpus, module-published documentation remains
available.

## Structured queries

`doc-lookup` returns entries for a name. `doc-entries` returns the complete
collection, optionally filtered by a predicate. The entry accessors
(`doc:names`, `doc:forms`, `doc:returns`, `doc:libraries`, `doc:source`,
`doc:chapter`, `doc:url`, `doc:description`) expose each field, allowing the
manual to be queried by ordinary Scheme code.

```scheme
(describe 'eq-hashtable-ref)
(doc-lookup 'lambda)
```

## Publishing module documentation

Modules add entries with `register-descriptions!`. Each entry has this shape:

```scheme
(names forms returns libraries source chapter url description)
```

Registrations belong to the calling module. Reloading retracts its previous
entries and installs the new collection transactionally, just like modes and
key bindings. A URL may be `#f`.

The `(edit)` module uses this mechanism for commands such as `replace!!` and
`replace-all!`. Registered forms also drive the grey parameter suggestion in
`M-x`, so newly documented procedures receive prompt hints automatically.
