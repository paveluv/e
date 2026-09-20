# Describe and reference data

## Interactive commands

`C-h f` opens M-x with `(describe:show! ` typed, so the documented name
completes like any argument, and Enter shows its page. `M-.` describes the symbol at point in a Scheme buffer. `C-h k`
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

Documented definitions need none of this: their edocs give the same
entries. A few modules still register prose entries for their apps.
Registered forms also drive the grey parameter suggestion in
`M-x`, so newly documented procedures receive prompt hints automatically.

### Documented definitions

A library documents its exports where it defines them. A documented
library is written with `elibrary`; its file starts with the import that
brings the form in:

```scheme
(import (only (edoc) elibrary))
(elibrary (datum)
  (export copy invalid?)
  (import (rnrs))

  (edoc "Data was not plain protocol data.")
  (define-condition-type &invalid &error make-invalid invalid?)

  (edoc "A deep copy of plain protocol data, sharing nothing mutable."
        (value datum "the data")
        (copy-leaf procedure "(copy-leaf leaf) giving a leaf's copy; omitted, an opaque leaf is invalid")
        (returns datum))
  (define copy (case-lambda [(value) ...] [(value copy-leaf) ...]))
  ...)
```

An `edoc` form annotates the definition that follows it, which stays an
ordinary `define`, `define-syntax`, `define-record-type` or
`define-condition-type`; `(edoc name summary clause ...)` documents a name
some other form defines. Every export the body defines must have an edoc,
or expansion fails naming the export. Re-exported imports and aliases such
as `(define x other:y)` take their documentation from their origin.

The annotations are checked while the library expands. The summary is a
string. For a procedure, every formal has exactly one clause `(name type
note ...)` and no other name appears; a rest parameter's type is a
`list-of`; a case-lambda's edoc names the formals of all its clauses, and
the reader gets one signature per clause. A parameter or another value
takes one `(value type note ...)` clause for what it holds, and a
procedure built by an expression takes argument clauses like a lambda's.
A record's edoc names every field, and derives edocs for the constructor,
the predicate and the field procedures; a record with a protocol or a
parent documents its constructor only through a `(constructor field ...)`
clause naming the arguments among the fields. A condition type's edoc
names its fields the same way. A keyword's clauses name the parts of its
form. Three clauses are declarations rather than formals: `(prompts)` for
a procedure that waits for a key, `(effects internal)` for a query whose
only changes are its own caches, and `(effects remote)` for a transport
whose effect is the message's; the effects check in
[Modules](MODULES.md) reads them. `returns` appears at most once, and every type is in the vocabulary:
the editor's notions `file`, `directory`, `buffer`, `window`, `region`,
`position`, `command`, `symbol`, `key`, `mode`, `style`; the language's
`string`, `char`, `integer`, `number`, `boolean`, `list`, `pair`,
`vector`, `bytevector`, `hashtable`, `port`, `procedure`, `thunk`,
`condition`, `datum`, `any`; `#f` for unions such as `(or string #f)`; and
the compounds `(one-of literal ...)`, `(or type ...)`, `(list-of type)` and
`(record name)` for an instance of a record type.

Types are data. When the library initializes, each name a clause uses
resolves to a type record with prose, a predicate and, optionally, a
completer, a reader and a writer; an unknown name is an error then, and
`tools/edoc-coverage.sps` reports one statically. The language's types come
predefined in `(edoc)`; the editor's notions are defined by the libraries
that own them, `buffer`, `window`, `region`, `actor` and `head` in `(literal)`, `file` in `(file)`, `mode`
in `(mode)`, `key` in `(keymap)`, `style` in `(style)`, with a form in the
body:

```scheme
(edoc-type buffer "a live buffer, spelled (buffer \"name\")"
  (predicate live-buffer?)
  (complete (lambda (partial) (map (lambda (b) (cons b (buffer-details b))) buffers)))
  (read lookup-buffer)
  (write (lambda (b) (format "(buffer ~s)" (head:buffer-name b)))))
```

The completer gives `(value . hint)` pairs for a partial text, the writer
spells a value as the expression denoting it, and a record documented in an
elibrary registers its predicate for `(record name)` by itself. M-x uses the
completers and writers to complete arguments by type; see
[Evaluation](EVAL.md). `type-accepts?`, `type-completions`, `type-spelling`
and `type-prose` work over the compound forms too.

When the library is initialized the edocs are attached to the objects they
document; a keyword, a record or condition type, and a value without
identity, such as a number, are recorded under their names. `edoc-of` reads
an object's signatures back, `edoc-named` those recorded under a name, and
`edoc-entry` shapes signatures as an entry in the format above, under the
source `edoc` and the chapter "Documented definitions"; a value's type
stands where a return would. A head sends those entries with every
describe query for the top-level definitions the registry does not already
cover, so a documented definition gets a describe page and the `M-x`
parameter suggestion without a `doc:register!` batch. The types are meant
for tooling: they describe what an argument is, and later choose how it is
completed; they are never checked at run time. Every library is an
`elibrary` except `(edoc)` itself, which documents its own exports with
the same checks; `tools/edoc-coverage.sps` reports, library by library,
which exports carry an edoc and what kind of definition the others are,
`--list` names them, and `--effects` checks every documented name's bang
against what its body reaches.
