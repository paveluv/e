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

`reference:lookup` returns entries for a name. `reference:entries` returns the complete
collection, optionally filtered by a predicate. The entry accessors
(`doc:names`, `doc:forms`, `doc:returns`, `doc:libraries`, `doc:source`,
`doc:chapter`, `doc:url`, `doc:description`) expose each field, allowing the
manual to be queried by ordinary Scheme code.

Entry records are immutable. Query result lists, accessor results and
`doc:to-datum` results are owned snapshots: changing their lists or strings
does not change later queries or the entry itself.

```scheme
(describe:this eq-hashtable-ref)
(reference:lookup 'lambda)
```

Code that needs documentation data can import `(reference)` under `reference:`.
Its `lookup`, `entries`, and `browser-url` operations own corpus queries;
`reference:fetch!` is the same operation as the interactive
`describe:fetch-data!` command. These queries include both the downloaded corpus
and current `doc:` registrations.

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

`doc:make` takes the eight fields as arguments; `doc:from-datum` takes one
entry list. Both own their input data, as does `doc:register!`. Inputs must
be finite plain data; cycles and runtime objects such as procedures are
rejected. A bad registration batch publishes no entries. Update documentation
through module registration or reload; changing a retained input has no effect.

The `(edit)` module uses this mechanism for commands such as `replace!!` and
`replace-all!`. Registered forms also drive the grey parameter suggestion in
`M-x`, so newly documented procedures receive prompt hints automatically.

### Documented definitions

A procedure can carry its documentation in its own definition instead:

```scheme
(edefine (visit-file! path)
  (edoc "Visit a file in the current window, creating or reusing its buffer."
        (path file "the file to visit")
        (returns buffer))
  ...)
```

`edefine`, from the `(edoc)` library, binds the procedure and keeps the
`edoc` form at the head of its body as a quoted datum: a constant the body
discards, so it costs nothing to run, while the procedure's recorded source
keeps it. `edefine` also accepts a `case-lambda` whose clauses each open with
an `edoc`. The form is checked while the module expands: the summary is a
string, every formal has exactly one clause `(name type note ...)` and no
other name appears, a rest parameter's type is a `list-of`, `returns` appears
at most once, and every type is in the vocabulary: the editor's notions
`file`, `directory`, `buffer`, `window`, `command`, `symbol`, `key`, `mode`,
`style`; the language's `string`, `char`, `integer`, `number`, `boolean`,
`list`, `pair`, `vector`, `bytevector`, `hashtable`, `port`, `procedure`,
`thunk`, `condition`, `datum`, `any`; and the compounds `(one-of literal
...)`, `(or type ...)`, `(list-of type)` and `(record name)` for an instance
of a record type. An `edoc` anywhere else is a syntax error.

Definitions without a lambda body carry an `edoc` too. `(edefine name (edoc
summary clause ...) expression)` attaches the datum to the value when it is
defined: a parameter or another value takes one `(value type note ...)`
clause for what it holds, and a procedure built by an expression takes
argument clauses like a lambda's. A value without identity, such as a
number, is recorded under its name. `(edefine-record-type spec (edoc summary
(field type note ...) ...) clause ...)` is a `define-record-type` whose
constructor, predicate and field procedures all carry edocs derived from the
record's one form; every field has exactly one clause, and a record with a
protocol or a parent documents no constructor. `(edefine-syntax name (edoc
summary clause ...) transformer)` records a keyword's edoc under its name,
the clauses naming the form's parts.

`edoc-of` reads an object's signatures back, from its attached edoc or from
a procedure's source, `edoc-named` those recorded under a name, and
`edoc-entry` shapes signatures as an entry in the format above, under the
source `edoc` and the chapter "Documented definitions"; a value's type stands
where a return would. A head sends those entries with every describe query
for the top-level definitions the registry does not already cover, so a
documented definition gets a describe page and the `M-x` parameter
suggestion without a `doc:register!` batch. The types are meant for
tooling: they describe what an argument is, and later choose how it is
completed; they are never checked at run time. `tools/edoc-coverage.sps`
reports, library by library, which exports carry an edoc and what kind of
definition the others are; `--list` names them.
