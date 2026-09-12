;; Client implementation of the store seam. Only immutable text and owned
;; metadata are cached; the base admits all writes and computes all history.
;; A change notice marks an entry stale; the next read asks the base for the
;; chain since the cached revision and applies it, so ordinary edits cost a
;; delta on the wire, never the buffer's text.
(library (store)
  (export create! visit! delete! discard! prepare-close reset! rename! buffer-list exists? visible? buffer-name find-named find-file
          snapshot snapshot-state snapshot-since revision line-count line extract
          property properties set-property! set-properties!
          edit! edit-with-snapshot! history-step! undo-authors history blame marks set-marks!
          validate-properties validate-edit-context watch! unsubscribe!)
  (import (chezscheme)
          (prefix (client) client:) (prefix (kernel) kernel:)
          (prefix (identity) identity:) (prefix (property) property:)
          (prefix (datum) datum:) (prefix (text) text:))

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

  (define (read-state id basis)
    ;; Refresh the cache from the authority and return
    ;; (label text revision facts changes) or #f. Changes are the decoded
    ;; chain since the caller's basis, #f when it is not continuous.
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
  (define (buffer-list) (client:request 'buffers))
  (define (prepare-close)
    (error 'prepare-close "an attached head does not own the base lifetime"))
  (define (exists? id) (and (cached id) #t))
  (define (buffer-name id) (string-copy (car (required id))))
  (define (find-named name)
    (find (lambda (id) (equal? (buffer-name id) name)) (buffer-list)))
  (define (find-file path)
    (let ([id (client:request 'find-file path)])
      (when id (forget! id))
      id))
  (define (properties id) (datum:copy (cadddr (required id))))
  (define property
    (case-lambda
      [(id key) (property id key #f)]
      [(id key fallback)
       (cond [(assq key (cadddr (required id))) => (lambda (fact) (datum:copy (cdr fact)))]
         [else fallback])]))
  (define (visible? actor id)
    (and (exists? id) (identity:in-audience? actor (property id 'audience 'all))))

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
  (define snapshot-state
    (case-lambda
      [(id) (snapshot-state id #f)]
      [(id basis)
       (let ([state (capture id basis)])
         (if basis
             (values (cadr state) (caddr state) (datum:copy (cadddr state)) (list-ref state 4))
             (values (cadr state) (caddr state) (datum:copy (cadddr state)))))]))
  (define (snapshot id)
    (let ([state (capture id #f)]) (values (cadr state) (caddr state))))
  (define (snapshot-since id basis)
    ;; The per-frame catch-up: no facts copy, which would include a file
    ;; baseline the size of the buffer.
    (let ([state (capture id basis)]) (values (cadr state) (caddr state) (list-ref state 4))))
  (define (revision id) (caddr (required id)))
  (define (line-count id) (vector-length (cadr (required id))))
  (define (line id row) (vector-ref (cadr (required id)) row))
  (define (extract id span) (text:extract (cadr (required id)) span))

  (define (check-actor actor)
    (unless (equal? actor (client:identity)) (error 'store "an attached head writes as itself")))
  (define (mutate actor id operation args)
    (check-actor actor)
    (let ([result (apply client:request operation id args)])
      (stale! id 'facts)
      result))
  (define (create! actor name lines . facts)
    (check-actor actor)
    (apply client:request 'create name lines facts))
  (define (visit! actor name lines facts)
    (check-actor actor)
    (let ([result (client:request 'visit name lines facts)])
      (forget! (car result))
      (apply values result)))
  (define (delete! actor id) (mutate actor id 'delete '()) (void))
  (define (discard! actor id revision facts) (mutate actor id 'discard (list revision facts)))
  (define (reset! actor id lines . options)
    (mutate actor id 'reset (cons lines options)))
  (define (rename! actor id name) (mutate actor id 'rename (list name)))
  (define (set-properties! actor id updates . options)
    (mutate actor id 'properties (cons updates options)))
  (define (set-property! actor id key value) (set-properties! actor id (list (cons key value))))

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
               (not (and context (>= (length context) 3)
                         (or (pair? (caddr context))
                             (and (>= (length context) 4) (pair? (cadddr context)))))))
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
  (define (edit! actor id basis span replacement . options)
    (let-values ([(status detail) (apply edit-with-snapshot! actor id basis span replacement options)])
      (values status (if (eq? status 'applied) (car detail) detail))))
  (define (history-step! actor id direction scope . access)
    (unless (<= (length access) 1) (error 'history-step! "expected one write access"))
    (apply values (mutate actor id 'history-step (list direction scope))))
  (define (undo-authors id) (client:request 'undo-authors id))
  (define (history id . count) (apply client:request 'history id count))
  (define (blame id . count)
    (map (lambda (entry) (cons (text:datum->span (car entry)) (cdr entry)))
      (apply client:request 'blame id count)))
  (define (set-marks! actor id basis updates drops)
    (check-actor actor)
    (apply values
      (client:request 'marks id basis
        (map (lambda (entry)
               (cons (car entry) (if (text:span? (cdr entry))
                                   (list 'span (text:span->datum (cdr entry))) (cdr entry)))) updates)
        drops)))
  (define (marks actor id)
    (check-actor actor)
    (map (lambda (entry)
           (cons (car entry)
             (if (and (pair? (cdr entry)) (eq? (cadr entry) 'span))
                 (text:datum->span (caddr entry)) (cdr entry))))
      (client:request 'read-marks id)))
)
