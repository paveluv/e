(import (only (foundation edoc) elibrary))
(elibrary (service document)
  (export acquire! check! reload! reread! save! save-output!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Acquire a file or directory in the base without choosing its destination. Returns (directory path) or (buffer id admitted? path diagnostic); disk contents stay in the base."
        (actor actor "connection supplies attribution") (path string "absolute path")
        (proposal (list-of any) "optional creation witness") (returns list))
  (define (acquire! actor path . proposal)
    (unless (equal? actor (client:identity)) (error 'acquire! "actor differs from connection identity"))
    (apply client:request 'acquire path proposal))

  (define (request actor id operation)
    (unless (equal? actor (client:identity)) (error 'document "actor differs from connection identity"))
    (client:request operation id))

  (edoc "Save a shared document at the base, including merge, backup and atomic file/mode adoption. Returns (saved/unchanged/refused/failed message). Disk contents remain in the base; hooks belong to the caller."
        (actor actor "connection supplies attribution") (id integer "document identity")
        (path string "canonical target") (adoption list "(reviewed-first-line detected-mode-name-or-false)") (returns list))
  (define (save! actor id path adoption)
    (unless (equal? actor (client:identity)) (error 'save! "actor differs from connection identity"))
    (client:request 'document-save id path adoption))

  (edoc "Save detached legacy local output through the base; a saved receipt includes facts for guarded local adoption. No shared shadow document is created."
        (actor actor "connection supplies attribution") (path string "canonical target")
        (text vector "local lines") (facts list "coherent local facts")
        (adoption list "(reviewed-first-line detected-mode-name-or-false)") (returns list))
  (define (save-output! actor path text facts adoption)
    (unless (equal? actor (client:identity)) (error 'save-output! "actor differs from connection identity"))
    (client:request 'document-save-output path text facts adoption))

  (edoc "Reload a document's file at the base as an undoable merge, guarded against concurrent edits and retargeting; return status and detail. Disk contents remain in the base."
        (actor actor "connection supplies attribution") (id integer "buffer identity"))
  (define (reload! actor id) (apply values (request actor id 'document-reload)))

  (edoc "Reread a document's file at the base as one undoable replacement, guarded against concurrent edits and retargeting; return status and detail. Disk contents remain in the base."
        (actor actor "connection supplies attribution") (id integer "buffer identity"))
  (define (reread! actor id) (apply values (request actor id 'document-reread)))

  (edoc "Check a document's disk content against its baseline in the base. Equal content updates only its reviewed stamp. This is a reload hint, not an overwrite authorization."
        (actor actor "connection supplies attribution") (id integer "buffer identity") (returns boolean))
  (define (check! actor id) (request actor id 'document-check)))
