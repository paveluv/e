;; Only an installed broker can address canonical base resources.
(import (only (foundation edoc) elibrary))
(elibrary (service resource)
  (export commit! edit! install! read)
  (import (except (chezscheme) read))
  (define broker #f)

  (edoc "Install this worker's resource broker once, before importing user libraries."
        (procedure procedure "operation and arguments -> portable result"))
  (define (install! procedure)
    (when broker (error 'install! "resource broker is already installed"))
    (set! broker procedure))
  (define (request op args)
    (unless broker (error 'resource "no base resource broker is installed"))
    (apply broker op args))

  (edoc "Read a declared resource's revision and value from its owning base."
        (name symbol "recipe resource name") (returns list "(revision value)"))
  (define (read name) (request 'read (list name)))

  (edoc "Edit a declared text resource at an exact revision, under the active job's actor."
        (name symbol "recipe resource name") (revision integer "expected revision")
        (span list "(start-row start-column end-row end-column)") (lines (list-of string) "replacement text")
        (returns datum))
  (define (edit! name revision span lines) (request 'edit (list name revision span lines)))

  (edoc "Replace a declared data model's value at an exact revision; references and ownership stay with the base."
        (name symbol "recipe resource name") (revision integer "expected revision") (value datum "new value") (returns datum))
  (define (commit! name revision value) (request 'commit (list name revision value))))
