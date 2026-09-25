;; Client implementation of the store seam. Only immutable text and owned
;; metadata are cached; the base admits all writes and computes all history.
;; A change notice marks an entry stale; the next read asks the base for the
;; chain since the cached revision and applies it, so ordinary edits cost a
;; delta on the wire, never the buffer's text.
(import (only (foundation edoc) elibrary))
(elibrary (state store)
  (export blame buffer-list buffer-name conflicts create! delete! discard! edit! edit-with-snapshot! exists?
          extract find-file find-named history history-step! line line-count
          (rename (log-entries log)) marks properties property reload! rename! reset! resolve! revision rewrite!
          set-marks! set-properties! set-property! snapshot snapshot-since snapshot-state
          trash-retention undo-authors undo-labels unsubscribe! validate-edit-context validate-properties
          view visible? visit! watch!)
  (import (chezscheme)
          (prefix (core client) client:)
          (prefix (core identity) identity:)
          (prefix (core kernel) kernel:)
          (prefix (core property) property:)
          (prefix (foundation datum) datum:)
          (prefix (foundation text) text:))

  (define validate-properties property:validate)
  (define validate-edit-context property:edit-context)
  ;; id -> (label text revision facts), or #f for a buffer known to be absent
  (define cache (make-eqv-hashtable))
  ;; ids whose cached entry may lag the base: text (edits only, so only the
  ;; modification facts) or facts (anything)
  (define stale (make-eqv-hashtable))
  ;; id -> the recent applied chain, newest first, ending at the cached
  ;; revision: a catch-up from an older basis the ring still covers needs
  ;; no request. Bounded like the store's own delta log.
  (define chains (make-eqv-hashtable))
  (define chain-limit 256)
  (define watching? #f)

  (define (remember-chain! id have changes)
    ;; changes: decoded, oldest first, ending at the current revision
    (let ([fresh (reverse (filter (lambda (change) (> (car change) have)) changes))])
      (hashtable-set! chains id
        (let take ([rest (append fresh (hashtable-ref chains id '()))] [n 0] [out '()])
          (if (or (null? rest) (= n chain-limit)) (reverse out)
              (take (cdr rest) (+ n 1) (cons (car rest) out)))))))
  (define (chain-since id basis revision)
    ;; the retained chain after basis, oldest first, or #f
    (let take ([rest (hashtable-ref chains id '())] [out '()])
      (cond [(= (+ basis (length out)) revision)
             (if (or (null? out) (= (caar out) (+ basis 1))) out #f)]
            [(or (null? rest) (<= (car (car rest)) basis)) #f]
            [else (take (cdr rest) (cons (car rest) out))])))

  (define (entry id) (and (hashtable-contains? cache id) (hashtable-ref cache id #f)))
  (define (merge-facts fresh old)
    (append fresh (remp (lambda (fact) (assq (car fact) fresh)) old)))
  (define (stale! id level)
    (when (and (hashtable-contains? cache id)
               (not (eq? (hashtable-ref stale id #f) 'facts)))
      (hashtable-set! stale id level)))
  (define (mark-stale! pending)
    ;; A batch names changed ids, each flagged when its facts or lifecycle
    ;; changed; #f means anything may have changed.
    (if pending
        (for-each (lambda (change) (stale! (car change) (if (cdr change) 'facts 'text))) pending)
        (vector-for-each (lambda (id) (stale! id 'facts)) (hashtable-keys cache))))
  (define invalidation
    (kernel:call-with-runtime-registrations
      (lambda () (client:subscribe! 'changed mark-stale!))))

  (edoc "Subscribe to invalidations from the base: (values token take), waking the head on changes."
        (wake thunk "run when something changes"))
  (define (watch! wake)
    (client:set-wake! wake)
    (unless watching? (client:request 'watch-head) (set! watching? #t))
    (client:watch! 'changed wake))
  (define unsubscribe! client:unsubscribe!)

  (define (decode-changes changes)
    (and changes
         (map (lambda (change) (list (car change) (cadr change) (text:datum->delta (caddr change)))) changes)))

  (define (apply-chain text have changes)
    ;; The chain is contiguous and ends at the base's current text; entries
    ;; the cache already holds are skipped.
    (fold-left (lambda (text change)
                 (if (> (car change) have)
                     (let-values ([(next delta)
                                   (text:apply-edit text (text:delta-span (caddr change))
                                                    (text:delta-inserted (caddr change)))])
                       next)
                     text))
               text changes))

  (edoc "Refresh the cache from the authority: (label text revision facts changes), or #f for a buffer that is gone; the changes are the decoded chain since the caller's basis, #f when it is not continuous."
        (id integer "the buffer id")
        (basis (or integer #f) "the caller's revision")
        (returns (or list #f))
        (effects internal))
  (define (read-state id basis)
    ;; With cached text the request starts at the cached revision (or the
    ;; caller's earlier basis) and asks for a delta reply; with cached facts
    ;; too, only modification facts come back and merge into them.
    (let* ([old (entry id)]
           [have (and old (caddr old))]
           [ask (cond [(not have) basis] [(not basis) have] [else (min have basis)])]
           [mode (and have (if (eq? (hashtable-ref stale id 'text) 'facts) #t 'facts))]
           [state (client:request 'state id ask mode)])
      (hashtable-delete! stale id)
      (cond
        [(not state) (hashtable-set! cache id #f) #f]
        [else
         (let* ([changes (and ask (decode-changes (list-ref state 4)))]
                [text (or (cadr state) (apply-chain (cadr old) have changes))]
                [revision (caddr state)]
                [facts (if (and (eq? mode 'facts) (not (cadr state)))
                           (merge-facts (cadddr state) (cadddr old))
                           (cadddr state))])
           (cond [(not changes) (hashtable-delete! chains id)]
                 [(cadr state) (hashtable-set! chains id (reverse changes))]
                 [else (remember-chain! id have changes)])
           (hashtable-set! cache id (list (car state) text revision facts))
           (list (car state) text revision facts
                 (and basis changes (<= basis revision)
                      (filter (lambda (change) (> (car change) basis)) changes))))])))
  (define (cached id)
    (if (and (hashtable-contains? cache id) (not (hashtable-contains? stale id)))
        (hashtable-ref cache id #f)
        (and (read-state id #f) (hashtable-ref cache id #f))))
  (define (required id)
    (or (cached id) (error 'store "no buffer" id)))
  (define (forget! id)
    (hashtable-delete! cache id)
    (hashtable-delete! stale id)
    (hashtable-delete! chains id))

  (edoc "Every buffer's id."
        (returns (list-of integer)))
  (define (buffer-list)
    (client:request 'buffers))

  (edoc "How many days the base keeps a trashed buffer before deleting it."
        (returns integer))
  (define (trash-retention)
    (client:request 'trash-retention))

  (edoc "Whether a buffer id is live."
        (id integer "the buffer id")
        (returns boolean))
  (define (exists? id)
    (and (cached id) #t))

  (edoc "A copy of a buffer's name."
        (id integer "the buffer id")
        (returns string))
  (define (buffer-name id)
    (string-copy (car (required id))))

  (edoc "The id of the buffer with a name, or #f."
        (name string "the name")
        (returns (or integer #f)))
  (define (find-named name)
    (find (lambda (id) (equal? (buffer-name id) name)) (buffer-list)))

  (edoc "The id of the buffer visiting a file, or #f."
        (path file "the file")
        (returns (or integer #f))
        (effects internal))
  (define (find-file path)
    (let ([id (client:request 'find-file path)])
      (when id (forget! id))
      id))

  (edoc "Every fact of a buffer, copied."
        (id integer "the buffer id")
        (returns list))
  (define (properties id)
    (datum:copy (cadddr (required id))))

  (edoc "A buffer's fact, or a fallback when absent, #f by default."
        (id integer "the buffer id")
        (key symbol "the fact")
        (fallback any "the value when absent")
        (returns any))
  (define property
    (case-lambda
      [(id key)
       (property id key #f)]
      [(id key fallback)
       (cond [(assq key (cadddr (required id))) => (lambda (fact) (datum:copy (cdr fact)))]
         [else fallback])]))

  (edoc "Whether an actor is in a buffer's audience."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (returns boolean))
  (define (visible? actor id)
    ;; as the base decides it: a trashed buffer is visible to nobody
    (and (exists? id) (not (property id 'trashed #f))
         (identity:in-audience? actor (property id 'audience 'all))))

  (define (capture id basis)
    ;; -> (label text revision facts changes): explicit captures (file
    ;; checks and adoption) contact the authority. A catch-up from a basis
    ;; the fresh cache still covers is already coherent: the chain is
    ;; complete until a notice says otherwise. Facts are shared here;
    ;; readers that hand them out copy them.
    (let* ([old (entry id)]
           [chain (and basis old (not (hashtable-contains? stale id))
                       (<= basis (caddr old)) (chain-since id basis (caddr old)))])
      (if chain
          (append old (list chain))
          (or (read-state id basis) (error 'snapshot-state "no buffer" id)))))

  (edoc "A buffer's text, revision and facts from one read, and the changes since a basis when one is given: (values text revision facts [changes])."
        (id integer "the buffer id")
        (basis (or integer #f) "the earlier revision"))
  (define snapshot-state
    (case-lambda
      [(id)
       (snapshot-state id #f)]
      [(id basis)
       (let ([state (capture id basis)])
         (if basis
             (values (cadr state) (caddr state) (datum:copy (cadddr state)) (list-ref state 4))
             (values (cadr state) (caddr state) (datum:copy (cadddr state)))))]))

  (edoc "A buffer's text and revision: (values text revision)."
        (id integer "the buffer id"))
  (define (snapshot id)
    (let ([state (capture id #f)]) (values (cadr state) (caddr state))))

  (edoc "A buffer's text, revision and the changes since a basis: (values text revision changes)."
        (id integer "the buffer id")
        (basis (or integer #f) "the earlier revision"))
  (define (snapshot-since id basis)
    ;; The per-frame catch-up: no facts copy, which would include a file
    ;; baseline the size of the buffer.
    (let ([state (capture id basis)]) (values (cadr state) (caddr state) (list-ref state 4))))

  (edoc "A buffer's revision."
        (id integer "the buffer id")
        (returns integer))
  (define (revision id)
    (caddr (required id)))

  (edoc "How many lines a buffer has."
        (id integer "the buffer id")
        (returns integer))
  (define (line-count id)
    (vector-length (cadr (required id))))

  (edoc "One line of a buffer."
        (id integer "the buffer id")
        (row integer "the row")
        (returns string))
  (define (line id row)
    (vector-ref (cadr (required id)) row))

  (edoc "A span's content in a buffer, as lines."
        (id integer "the buffer id")
        (span (record span) "the span")
        (returns list))
  (define (extract id span)
    (text:extract (cadr (required id)) span))

  (define (check-actor actor)
    (unless (equal? actor (client:identity)) (error 'store "an attached head writes as itself")))
  (define (mutate actor id operation args)
    (check-actor actor)
    (let ([result (apply client:request operation id args)])
      (stale! id 'facts)
      result))

  (edoc "Create a buffer in the base with a name, lines and optional facts; its id."
        (actor actor "the actor identity")
        (name string "the name")
        (lines (or list vector) "the lines")
        (facts (list-of list) "a fact batch, at most one")
        (returns integer))
  (define (create! actor name lines . facts)
    (check-actor actor)
    (apply client:request 'create name lines facts))

  (edoc "Visit a file as a buffer in the base: (values id created?)."
        (actor actor "the actor identity")
        (name string "the name")
        (lines (or list vector) "the lines read")
        (facts list "the file facts"))
  (define (visit! actor name lines facts)
    (check-actor actor)
    (let ([result (client:request 'visit name lines facts)])
      (forget! (car result))
      (apply values result)))

  (edoc "Delete a buffer."
        (actor actor "the actor identity")
        (id integer "the buffer id"))
  (define (delete! actor id)
    (mutate actor id 'delete '()) (void))

  (edoc "Delete a buffer only while its reviewed revision and facts still hold; whether it was deleted."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (revision integer "the reviewed revision")
        (facts list "the reviewed facts")
        (returns boolean))
  (define (discard! actor id revision facts)
    (mutate actor id 'discard (list revision facts)))

  (edoc "Replace a buffer's baseline wholesale, with optional facts and a reviewed state; the new revision, or #f."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (lines (or list vector) "the lines")
        (options (list-of any) "facts, then a reviewed state")
        (returns (or integer #f)))
  (define (reset! actor id lines . options)
    (mutate actor id 'reset (cons lines options)))

  (edoc "Rename a buffer; the accepted name."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (name string "the wanted name")
        (returns string))
  (define (rename! actor id name)
    (mutate actor id 'rename (list name)))

  (edoc "Set facts of a buffer, optionally under a review and with a new name; whether accepted."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (updates list "(key . value) facts")
        (options (list-of any) "a fact review, then a new name")
        (returns boolean))
  (define (set-properties! actor id updates . options)
    (mutate actor id 'properties (cons updates options)))

  (edoc "Set one fact of a buffer."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (key symbol "the fact")
        (value datum "its value"))
  (define (set-property! actor id key value)
    (set-properties! actor id (list (cons key value))))

  (edoc "Apply an edit against a basis, acknowledged with (revision text changes edit-facts) that advances the head's cache."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (basis integer "the revision edited")
        (span (record span) "the span replaced")
        (replacement list "the replacement lines")
        (options (list-of any) "an edit context, then write access"))
  (define (edit-with-snapshot! actor id basis span replacement . options)
    ;; Ask for a delta receipt when the cache holds text at or past the
    ;; basis, then advance the cached text through the receipt's chain.
    ;; The cache stays stale for facts the commit may have changed.
    (unless (<= (length options) 2) (error 'edit! "expected context and write access"))
    (check-actor actor)
    (let* ([context (and (pair? options) (car options))]
           [old (entry id)]
           [have (and old (caddr old))]
           [delta? (and have (>= have basis))]
           [result (apply client:request 'edit id basis (text:span->datum span) replacement context
                     (if delta? '(#t) '()))]
           [status (car result)] [detail (cadr result)])
      ;; A plain delta receipt carries its modification facts; explicit
      ;; committed facts leave the entry stale.
      (if (and (eq? status 'applied) delta?
               (not (or (pair? (property:context-undo context)) (pair? (property:context-commit context)))))
          (hashtable-delete! stale id)
          (stale! id 'facts))
      (values status
        (if (eq? status 'applied)
            (let* ([revision (car detail)]
                   [changes (decode-changes (caddr detail))]
                   [text (or (cadr detail) (apply-chain (cadr old) have changes))]
                   [facts (and old (merge-facts (cadddr detail) (cadddr old)))])
              (when old
                (hashtable-set! cache id (list (car old) text revision facts))
                (if delta? (remember-chain! id have changes) (hashtable-set! chains id (reverse changes))))
              (list revision text changes (datum:copy (cadddr detail))))
            detail))))

  (edoc "Apply an edit against a basis: (values applied revision), or (values stale reason)."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (basis integer "the revision edited")
        (span (record span) "the span replaced")
        (replacement list "the replacement lines")
        (options (list-of any) "an edit context, then write access"))
  (define (edit! actor id basis span replacement . options)
    (let-values ([(status detail) (apply edit-with-snapshot! actor id basis span replacement options)])
      (values status (if (eq? status 'applied) (car detail) detail))))

  (edoc "Undo or redo in a buffer under a scope: (values status detail)."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (direction (one-of undo redo) "which way")
        (scope any "mine, all or (actor who)")
        (access (list-of any) "write access, at most one"))
  (define (history-step! actor id direction scope . access)
    (unless (<= (length access) 1) (error 'history-step! "expected one write access"))
    (apply values (mutate actor id 'history-step (list direction scope))))

  (edoc "The actors with retained live actions in a buffer, newest first."
        (id integer "the buffer id")
        (returns list))
  (define (undo-authors id)
    (client:request 'undo-authors id))

  (edoc "A buffer's newest applied edits as plain data, newest first."
        (id integer "the buffer id")
        (count (list-of integer) "how many, at most one")
        (returns list))
  (define (history id . count)
    (apply client:request 'history id count))

  (edoc "A buffer's undo groups from the base, newest first, (actor label) each."
        (id integer "the buffer id")
        (returns list))
  (define (undo-labels id)
    (client:request 'undo-labels id))

  (edoc "A buffer's retained log entries as data, newest first, (revision actor labels delta origin state) each, narrowed by a selector alist among count, actor, batch, since and until."
        (id integer "the buffer id")
        (selector (list-of any) "constraints, at most one alist")
        (returns list))
  (define (log-entries id . selector)
    (apply client:request 'log id selector))

  (edoc "A view of a buffer with entries disabled, from the base: (values text mapping conflicts)."
        (id integer "the buffer id")
        (disabled (list-of integer) "the revisions to disable")
        (returns any "(values text mapping conflicts)"))
  (define (view id disabled)
    (apply values (client:request 'view id disabled)))

  (edoc "Disable entries of a buffer for everyone through the base: (values status detail)."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (disabled (list-of integer) "the revisions to disable")
        (access (list-of any) "write access, at most one"))
  (define (rewrite! actor id disabled . access)
    (unless (<= (length access) 1) (error 'rewrite! "expected one write access"))
    (apply values (mutate actor id 'rewrite (list disabled))))

  (edoc "Reload a buffer from its file through the base: (values status detail), applied with (revision conflicts)."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (lines (or list vector) "the disk's lines")
        (facts list "the facts to commit")
        (access (list-of any) "write access, at most one"))
  (define (reload! actor id lines facts . access)
    (unless (<= (length access) 1) (error 'reload! "expected one write access"))
    (apply values (mutate actor id 'reload (list lines facts))))

  (edoc "Settle a reload conflict through the base: (values status detail)."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (revision integer "the conflicted entry")
        (choice any "disk, mine or the replacement lines")
        (access (list-of any) "write access, at most one"))
  (define (resolve! actor id revision choice . access)
    (unless (<= (length access) 1) (error 'resolve! "expected one write access"))
    (apply values (mutate actor id 'resolve (list revision choice))))

  (edoc "A buffer's pending reload conflicts from the base, (entry disk inverse) revisions each."
        (id integer "the buffer id")
        (returns list))
  (define (conflicts id)
    (client:request 'conflicts id))

  (edoc "A buffer's newest edits with their spans in the current text: (span actor revision) each."
        (id integer "the buffer id")
        (count (list-of integer) "how many, at most one")
        (returns list))
  (define (blame id . count)
    (map (lambda (entry) (cons (text:datum->span (car entry)) (cdr entry)))
      (apply client:request 'blame id count)))

  (edoc "Set and drop this head's marks in a buffer against a basis: (values applied revision) or (values stale revision)."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (basis (or integer #f) "the revision the positions describe")
        (updates list "(name . position) marks")
        (drops list "names to remove"))
  (define (set-marks! actor id basis updates drops)
    (check-actor actor)
    (apply values
      (client:request 'marks id basis
        (map (lambda (entry)
               (cons (car entry) (if (text:span? (cdr entry))
                                   (list 'span (text:span->datum (cdr entry))) (cdr entry)))) updates)
        drops)))

  (edoc "This head's marks in a buffer, (name . position) each."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (returns list))
  (define (marks actor id)
    (check-actor actor)
    (map (lambda (entry)
           (cons (car entry)
             (if (and (pair? (cdr entry)) (eq? (cadr entry) 'span))
                 (text:datum->span (caddr entry)) (cdr entry))))
      (client:request 'read-marks id)))
)
