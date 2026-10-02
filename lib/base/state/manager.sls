;; Default window topology is a view tree, not a second layout registry.
(import (only (foundation edoc) elibrary))
(elibrary (state manager)
  (export close! create! current document documents find-app link! links numbered open-document! resize! return! select! set-display! split! unlink! upgrade windows)
  (import (chezscheme) (prefix (core descriptor) descriptor:) (prefix (core editor-schema) editor-schema:)
          (prefix (core handle) handle:) (prefix (core identity) identity:) (prefix (core kernel) kernel:)
          (prefix (state model) model:) (prefix (state store) store:) (prefix (state terminal-state) terminal-state:) (prefix (state view) view:))
  (define copying
    (kernel:call-with-runtime-registrations
      (lambda ()
        (for-each (lambda (kind) (view:register-copy! (car kind) (cadr kind)
                                   (lambda (options state mapped)
                                     (values
                                       (map (lambda (p)
                                              (case (car p)
                                                [(selected) (cons 'selected (mapped (cdr p)))]
                                                [(links) (cons 'links (map (lambda (l) (list (mapped (car l)) (mapped (cadr l)) (caddr l))) (cdr p)))]
                                                [(presentations origins) (cons (car p) (map (lambda (p) (map mapped p)) (cdr p)))]
                                                [else p])) options)
                                       state)))) '((window-manager 2) (window 1))))))
  (define (field r k) (cdr (assq k r)))
  (define (value r) (field r 'value))
  (define (options r) (descriptor:options (value r)))
  (define (option r k default) (cond [(assq k (options r)) => cdr] [else default]))
  (define (with-option r k v) (cons (cons k v) (remp (lambda (p) (eq? k (car p))) (options r))))
  (define (display-options r) (filter (lambda (p) (memq (car p) '(wrap line-numbers scrollbar))) (options r)))
  (define (merge-options old changes)
    (append changes (remp (lambda (p) (assq (car p) changes)) old)))
  (define (record rows id) (or (find (lambda (r) (and r (equal? id (field r 'id)))) rows) (error 'manager "view is unavailable" id)))
  (define (kind? r kind)
    (and r (eq? (field r 'kind) 'widget-view) (= (field r 'schema) 3) (descriptor:valid? (value r))
      (= (descriptor:schema (value r)) (if (memq kind '(window-manager window-split)) 2 1)) (eq? (descriptor:kind (value r)) kind)))
  (define (children r) (descriptor:children (value r)))
  (define (child-ids r) (map cadr (children r)))
  (define (weight? n) (and (real? n) (rational? n) (> n 0)))
  (define (window-ids rows manager)
    (let walk ([id manager])
      (let ([r (record rows id)])
        (cond [(kind? r 'window) (list id)]
          [(or (kind? r 'window-manager) (kind? r 'window-split)) (apply append (map walk (child-ids r)))]
          [else (error 'manager "invalid window topology" id)]))))
  (define (capture actor manager)
    (unless (descriptor:head? actor) (error 'manager "expected a head actor"))
    (let* ([root (let climb ([id manager] [seen '()])
                   (when (member id seen) (error 'manager "cyclic parent chain"))
                   (let ([d (view:snapshot id)])
                     (unless d (error 'manager "view is unavailable" id))
                     (if (descriptor:parent d) (climb (descriptor:parent d) (cons id seen)) id)))]
           [tree (view:tree root)] [rows (map caddr (cadr (model:snapshots (map car tree))))]
           [m (record rows manager)])
      (unless (and (for-all (lambda (r d) (and r (equal? (value r) (cdr d)))) rows tree)
                   (kind? m 'window-manager) (equal? actor (option m 'head #f))
                   (equal? (map car (children m)) '(layout)))
        (error 'manager "manager changed or belongs to another head" manager))
      (let ([d (value (record rows root))])
        (unless (for-all (lambda (r) (equal? (descriptor:owner (value r)) (descriptor:owner d))) rows)
          (error 'manager "incoherent composition ownership"))
        (when (and (descriptor:owner d) (not (equal? actor (descriptor:owner d))))
          (error 'manager "composition is owned by another head")))
      (let ([numbers '()])
        (let walk ([id (car (child-ids m))])
          (let ([r (record rows id)])
            (unless (equal? manager (field r 'scope)) (error 'manager "topology has another lifetime owner" id))
            (cond [(kind? r 'window)
                   (let ([n (option r 'number #f)])
                     (unless (and (integer? n) (exact? n) (> n 0) (not (memv n numbers)))
                       (error 'manager "invalid or duplicate window number" n))
                     (set! numbers (cons n numbers)))]
              [(kind? r 'window-split)
               (unless (and (memq (option r 'axis #f) '(x y)) (equal? (map car (children r)) '(first second))
                         (equal? (map caddr (children r)) '((grow 1) (grow 1)))
                         (list? (descriptor:state (value r))) (= (length (descriptor:state (value r))) 2)
                         (for-all weight? (descriptor:state (value r))))
                 (error 'manager "invalid split" id))
               (for-each walk (child-ids r))]
              [else (error 'manager "invalid topology node" id)]))))
      rows))
  (define (require-window rows manager id)
    (unless (member id (window-ids rows manager)) (error 'manager "window is not in this manager" id))
    (record rows id))
  (define (active r) (cond [(assq 'document (children r)) => cadr] [else #f]))
  (define (active-document w)
    (cond [(find (lambda (p) (equal? (cadr p) (active w))) (option w 'presentations '())) => car] [else #f]))
  (define (document-of w r)
    (car (find (lambda (p) (equal? (cadr p) (field r 'id))) (option w 'presentations '()))))
  (define (retained rows w)
    (let* ([ps (option w 'presentations '())]
           [records (map caddr (cadr (model:snapshots (map cadr ps))))])
      (filter values
        (map (lambda (p r)
               (and r
                 (begin
                   (unless (and
                             (equal? (field r 'scope) (field w 'id))
                             (member (cadr p) (descriptor:owned (value w)))
                             (or (and (handle:buffer? (car p)) (or (kind? r 'editor) (kind? r 'terminal))
                                   (equal? (descriptor:source (value r)) (car p))
                                   (or (kind? r 'terminal) (and (null? (children r)) (null? (descriptor:owned (value r))))))
                               (and (equal? (car p) (field r 'id)) (eq? (field r 'kind) 'widget-view)
                                 (= (field r 'schema) 3) (descriptor:valid? (value r))))
                             (if (equal? (cadr p) (active w))
                               (equal? r (record rows (cadr p)))
                               (and (not (descriptor:parent (value r))) (not (descriptor:owner (value r))))))
                     (error 'manager "retained presentation changed or belongs elsewhere" p))
                   r))) ps records))))
  (define (with-retained rows records)
    (fold-left (lambda (rows r)
                 (let ([old (find (lambda (old) (equal? (field old 'id) (field r 'id))) rows)])
                   (when (and old (not (equal? old r))) (error 'manager "presentation changed; retry"))
                   (if old rows (append rows (list r))))) rows records))
  (define (presentation-tree id)
    (let* ([tree (view:tree id)] [rows (map caddr (cadr (model:snapshots (map car tree))))])
      (unless (and (pair? rows) (for-all (lambda (r d) (and r (equal? (value r) (cdr d)))) rows tree))
        (error 'manager "presentation changed; retry"))
      rows))
  (define (presentations-options w ps origin)
    (cons (cons 'presentations ps)
      (cons (cons 'owned (append (map cadr ps)
                           (filter (lambda (id) (not (member id (append (map cadr ps) (map cadr (option w 'presentations '()))))))
                             (descriptor:owned (value w)))))
        (cons (cons 'origins
                (filter (lambda (p) (and (assoc (car p) ps) (or (not (cadr p)) (assoc (cadr p) ps))))
                  (if origin (cons origin (remp (lambda (p) (equal? (car p) (car origin))) (option w 'origins '())))
                    (option w 'origins '()))))
          (remp (lambda (p) (memq (car p) '(presentations owned origins))) (options w))))))
  (define (live-document? actor facts)
    (and facts (not (field facts 'internal)) (not (field facts 'trashed)) (not (field facts 'backup))
      (identity:in-audience? actor (field facts 'audience))))
  (define (listed-app? actor d)
    (and d (cond [(assq 'catalogue (descriptor:options d)) => cdr] [else #f])
      (identity:in-audience? actor (cond [(assq 'audience (descriptor:options d)) => cdr] [else 'all]))))
  (define (available-document? actor document)
    (if (handle:buffer? document)
      (live-document? actor (cadar (cadr (store:metadata (list document)))))
      (listed-app? actor (view:snapshot document))))
  (define (fence d)
    (descriptor:with d (list (cons 'generation (+ 1 (descriptor:generation d))) '(sequence . 0))))
  (define (placement rows w ps target focus? origin)
    ;; One placement transition for opening and lifecycle fallback. Unchanged
    ;; witnesses do not invalidate other panes or their in-flight interaction.
    (let* ([window (field w 'id)] [old (active w)]
           [subtree (lambda (root)
                      (let walk ([id root])
                        (let ([r (find (lambda (r) (equal? id (field r 'id))) rows)])
                          (if r (cons id (apply append (map walk (child-ids r)))) '()))))]
           [previous (subtree old)] [next-tree (subtree target)])
      (map (lambda (r)
             (let* ([id (field r 'id)] [d (value r)]
                    [next
                     (cond [(equal? id window)
                            (descriptor:with d
                              (list (cons 'children (append (if target (list (list 'document target '(grow 1))) '())
                                                            (remp (lambda (c) (eq? (car c) 'document)) (children w))))
                                (cons 'options (presentations-options w ps origin))))]
                       [(and (eq? r (car rows)) focus?) (descriptor:with d (list (cons 'focus (or target window))))]
                       [(equal? old target) d]
                       [(member id previous)
                        (descriptor:with d (append '((owner . #f) (focus . #f)) (if (equal? id old) '((parent . #f)) '())))]
                       [(member id next-tree)
                        (descriptor:with d (append (list (cons 'owner (descriptor:owner (value w))) '(focus . #f))
                                             (if (equal? id target) (list (cons 'parent window)) '())))]
                       [else d])])
               (change r (if (equal? d next) d (fence next))))) rows)))
  (define (placement-focus? rows manager window)
    (or (equal? (focused rows manager) window)
      (let ([focus (descriptor:focus (value (car rows)))])
        (let climb ([id focus] [seen '()])
          (and id (not (member id seen))
            (or (equal? id (active (record rows window)))
              (let ([r (find (lambda (r) (equal? id (field r 'id))) rows)])
                (and r (climb (descriptor:parent (value r)) (cons id seen))))))))
      (and (not (descriptor:focus (value (car rows)))) (equal? (selected rows manager) window))))
  (define (focused rows manager)
    (let* ([ids (window-ids rows manager)] [focus (descriptor:focus (value (car rows)))])
      (and focus
           (let climb ([id focus])
             (cond [(member id ids) id]
               [(find (lambda (r) (equal? id (field r 'id))) rows)
                => (lambda (r) (let ([parent (descriptor:parent (value r))]) (and parent (climb parent))))]
               [else #f])))))
  (define (selected rows manager)
    (let ([ids (window-ids rows manager)] [last (option (record rows manager) 'selected #f)])
      (or (focused rows manager) (and (member last ids) last) (car ids))))
  (define (spec scope d) (list 'widget-view 3 scope 'persistent (descriptor:references d) d))
  (define (window-descriptor parent number status)
    (descriptor:with (descriptor:make #f 'window 1 (list (cons 'number number)) '())
      (list (cons 'parent parent) (cons 'children (list (list 'status status 'fit))))))
  (define (status-descriptor window)
    (descriptor:with (descriptor:make #f 'window-status 1 '() '()) (list (cons 'parent window))))
  (define (change r d) (list (field r 'id) (field r 'revision) (descriptor:references d) d))
  (define (configure! actor rows id opts focus)
    ;; Preferences and focus do not transfer ownership of the entire tree.
    ;; Fence only the root interaction when focus changes; unrelated views
    ;; keep their leases and produce no model notifications or wire traffic.
    (let-values ([(status records)
                  (model:commit! actor
                    (map (lambda (r)
                           (let* ([d (value r)]
                                  [d (if (equal? id (field r 'id)) (descriptor:with d (list (cons 'options opts))) d)]
                                  [d (if (and focus (eq? r (car rows)) (not (equal? focus (descriptor:focus d))))
                                       (descriptor:with d (list (cons 'focus focus)
                                                            (cons 'generation (+ 1 (descriptor:generation d))) '(sequence . 0))) d)])
                             (change r d))) rows))])
      (unless (eq? status 'applied) (error 'manager "composition changed; retry the operation" status))))
  (define (replacement r children options) (list (field r 'id) children options))
  (define (replace-child r old new)
    (replacement r (map (lambda (c) (if (equal? old (cadr c)) (list (car c) new (caddr c)) c)) (children r)) (options r)))
  (define (commit! actor rows updates focus retired)
    ;; All captured descriptors are witnesses, including windows whose numbers
    ;; or links were inspected. Disjoint simultaneous splits cannot reuse a label.
    (let ([changes (map (lambda (r)
                          (let ([next (assoc (field r 'id) updates)])
                            (list (field r 'id) (field r 'revision)
                              (if next (cadr next) (children r)) (if next (caddr next) (options r))))) rows)]
          [leases (filter values
                    (map (lambda (r) (let ([d (value r)])
                                       (and (not (descriptor:parent d)) (descriptor:owner d)
                                         (list (field r 'id) (descriptor:generation d))))) rows))])
      (let-values ([(status descriptors)
                    (view:arrange! actor changes leases
                      (list (and focus (car focus)) (and focus (cadr focus))
                        retired))])
        (unless (eq? status 'applied) (error 'manager "topology changed; retry the operation" status)))))

  (edoc "Create a persistent manager with one empty window, numbered 1. Allocation creates no renderer or background service."
        (actor actor "owning head") (owner (or model #f) "lifetime owner, false for a session root") (returns model))
  (define (create! actor owner)
    (unless (descriptor:head? actor) (error 'create! "expected a head"))
    (car (model:allocate! actor 3
           (lambda (ids)
             (list (spec (or owner 'session) (descriptor:with (descriptor:make #f 'window-manager 2
                                                                (list (cons 'head actor) (cons 'selected (cadr ids)) '(links)) '())
                                               (list (cons 'children (list (list 'layout (cadr ids) '(grow 1)))))))
               (spec (car ids) (window-descriptor (car ids) 1 (caddr ids)))
               (spec (cadr ids) (status-descriptor (cadr ids))))))))

  (edoc "List a manager's stable window references in topology order." (actor actor "head") (manager model "manager view") (returns list))
  (define (windows actor manager) (window-ids (capture actor manager) manager))

  (edoc "Resolve a displayed manager-local number, or false. A reused number never reuses its old model identity."
        (actor actor "head") (manager model "manager view") (number integer "positive label") (returns (or model #f)))
  (define (numbered actor manager number)
    (let ([rows (capture actor manager)])
      (find (lambda (id) (equal? number (option (record rows id) 'number #f))) (window-ids rows manager))))

  (edoc "Resolve the focused descendant's window, falling back to the manager's saved selection while focus is elsewhere."
        (actor actor "head") (manager model "manager view") (returns model))
  (define (current actor manager) (selected (capture actor manager) manager))

  (edoc "Read the active document's stable identity, or false for an empty window. Retirement selects the most recent surviving retained document, never a same-name replacement."
        (actor actor "head") (manager model "manager") (window model "window") (returns (or buffer model #f)))
  (define (document actor manager window)
    (let ([rows (capture actor manager)]) (active-document (require-window rows manager window))))

  (edoc "List a window's retained documents in most-recently-opened order. Hidden presentations are unmounted."
        (actor actor "head") (manager model "manager") (window model "window") (returns list))
  (define (documents actor manager window)
    (let* ([rows (capture actor manager)] [w (require-window rows manager window)])
      (map (lambda (r) (document-of w r)) (retained rows w))))

  (edoc "Change a window's display preferences without moving focus or changing input ownership. Wrap preferences update every retained ordinary editor atomically; hidden presentations and future visits use the same window policy. Terminal and app-owned editors keep their own policy. No backend geometry is stored."
        (actor actor "head") (manager model "manager") (window model "window")
        (preferences list "alist: wrap and line-numbers accept default or booleans; scrollbar also accepts left, right or auto"))
  (define (set-display! actor manager window preferences)
    (unless (and (list? preferences)
              (for-all (lambda (p) (and (pair? p)
                                     (case (car p)
                                       [(wrap line-numbers) (memq (cdr p) '(default #t #f))]
                                       [(scrollbar) (memq (cdr p) '(default #t #f left right auto))]
                                       [else #f]))) preferences)
              (let unique ([rest preferences])
                (or (null? rest) (and (not (assq (caar rest) (cdr rest))) (unique (cdr rest))))))
      (error 'set-display! "invalid display preferences" preferences))
    (let* ([rows (capture actor manager)] [w (require-window rows manager window)]
           [saved (retained rows w)] [rows (with-retained rows saved)]
           [editors (map (lambda (r) (field r 'id)) (filter (lambda (r) (and (kind? r 'editor) (handle:buffer? (document-of w r)))) saved))]
           [wrap (assq 'wrap preferences)])
      (let-values ([(status records)
                    (model:commit! actor
                      (map (lambda (r)
                             (change r
                               (cond [(equal? window (field r 'id))
                                      (descriptor:with (value r) (list (cons 'options (merge-options (options r) preferences))))]
                                 [(and wrap (member (field r 'id) editors))
                                  (descriptor:with (value r) (list (cons 'options (with-option r 'wrap (cdr wrap)))))]
                                 [else (value r)]))) rows))])
        (unless (eq? status 'applied) (error 'set-display! "window changed; retry" status)))))

  (edoc "Find a retained app by its app-key, preferring the destination window and then other panes in topology order. Return (owning-window app) or false; callers explicitly fork another pane's app before placement. No separate app registry is maintained."
        (actor actor "head") (manager model "manager") (window model "preferred window") (key string "nonempty app key") (returns any))
  (define (find-app actor manager window key)
    (unless (and (string? key) (> (string-length key) 0)) (error 'find-app "expected a nonempty app key"))
    (let ([rows (capture actor manager)])
      (require-window rows manager window)
      (let loop ([ids (cons window (remove window (window-ids rows manager)))])
        (and (pair? ids)
          (let ([matches (filter (lambda (r) (equal? key (option r 'app-key #f))) (retained rows (record rows (car ids))))])
            (when (> (length matches) 1) (error 'find-app "ambiguous app key" key))
            (if (null? matches) (loop (cdr ids)) (list (car ids) (field (car matches) 'id))))))))

  (define (place-document! actor manager rows w document enter? . prepared)
    ;; Entering admits a catalogue candidate and remembers the previous
    ;; document. Returning restores retention without rewriting that origin.
    (let* ([window (field w 'id)] [saved (retained rows w)]
           [known (find (lambda (r) (equal? document (document-of w r))) saved)]
           [presentation (cond [(pair? prepared) (car prepared)] [(model:reference? document) document]
                           [known (field known 'id)] [else #f])]
           [candidate (and presentation (presentation-tree presentation))]
           [rows (with-retained (with-retained rows saved) (or candidate '()))]
           [old (active w)]
           [focus? (placement-focus? rows manager window)]
           [owner (descriptor:owner (value w))] [scope-guards '()])
      (define (within-app? r)
        (let climb ([scope (field r 'scope)] [seen '()])
          (or (equal? scope presentation)
            (and (model:reference? scope) (not (member scope seen))
              (let ([r (or (find (lambda (r) (equal? scope (field r 'id))) rows) (model:snapshot scope))])
                (and r (eq? (field r 'persistence) 'persistent)
                  (begin
                    (unless (exists (lambda (row) (equal? scope (field row 'id))) rows)
                      (set! scope-guards (with-retained scope-guards (list r))))
                    (climb (field r 'scope) (cons scope seen)))))))))
      (define (updates target)
        (append (placement rows w
                  (cons (list document target)
                    (map (lambda (r) (list (document-of w r) (field r 'id)))
                      (filter (lambda (r) (not (equal? document (document-of w r)))) saved))) target focus?
                  (and enter? (model:reference? document) (not (equal? old target)) (list document (active-document w))))
          (map (lambda (r) (list (field r 'id) (field r 'revision) (field r 'references) (value r))) scope-guards)))
      (when (and candidate (model:reference? document))
        (let ([key (assq 'app-key (options (car candidate)))])
          (when key
            (unless (and (string? (cdr key)) (> (string-length (cdr key)) 0)
                      (not (exists (lambda (r) (and (not (equal? document (field r 'id)))
                                                 (equal? (cdr key) (option r 'app-key #f)))) saved)))
              (error 'open-document! "invalid or already retained app key" (cdr key))))))
      (when candidate
        (unless (and (or (handle:buffer? document) (not enter?) (listed-app? actor (value (car candidate)))) (equal? window (field (car candidate) 'scope))
                  (or (equal? old presentation) (not (descriptor:parent (value (car candidate)))))
                  (for-all (lambda (r) (and (eq? (field r 'persistence) 'persistent)
                                         (equal? (descriptor:owner (value r)) (and (equal? old presentation) owner))
                                         (null? (descriptor:cleanup (value r)))
                                         (or (eq? r (car candidate)) (within-app? r)))) candidate))
          (error 'open-document! "app must be prepared under this window and unmounted; fork it into the destination first" document)))
      (when (and old (not (exists (lambda (r) (equal? old (field r 'id))) saved)))
        (error 'open-document! "document slot holds an unmanaged presentation" old))
      (let ([id (if (or known candidate)
                  (let ([id presentation])
                    (unless (equal? id old)
                      (let-values ([(status records) (model:commit! actor (updates id))])
                        (unless (eq? status 'applied) (error 'open-document! "window changed before placement; retry"))))
                    id)
                  (let ([ids (model:allocate! actor 1
                               (lambda (ids)
                                 (list (spec window (descriptor:with (editor-schema:make document (list (cons 'wrap (option w 'wrap 'default))))
                                                      (list (cons 'parent window) (cons 'owner owner) (cons 'generation (if owner 1 0)))))))
                               (lambda (ids) (updates (car ids))))])
                    (unless ids (error 'open-document! "window changed before placement; retry"))
                    (car ids)))])
        ;; The text and model stores commit separately. A retirement notice
        ;; can precede placement; check again after publishing the reference.
        (when (and (handle:buffer? document) (not (available-document? actor document)))
          (reconcile! actor manager)
          (error 'open-document! "document retired during placement" document))
        id)))

  (edoc "Open a buffer or prepared catalogue app in an explicit window. Buffers retain independent editor or terminal presentations; opening a terminal never starts a process. Apps must already have this window's lifetime and a persistent unmounted subtree. Fork into the destination first for an independent presentation. Placement, recency, focus and app origins commit together."
        (actor actor "head") (manager model "manager") (window model "window") (document (or buffer model) "catalogue text or prepared app view") (returns model))
  (define (open-document! actor manager window document)
    (unless (or (handle:buffer? document) (model:reference? document)) (error 'open-document! "expected a document reference"))
    (unless (available-document? actor document) (error 'open-document! "document is unavailable" document))
    (let* ([rows (capture actor manager)] [w (require-window rows manager window)]
           [app (and (handle:buffer? document) (store:property document 'app #f))])
      (if (and app (eq? (cadr app) 'terminal)
            (not (exists (lambda (r) (equal? document (document-of w r))) (retained rows w))))
        (let ([id (terminal-state:create! actor window document)])
          (guard (ex [else (let ([d (view:snapshot id)])
                             (when (and d (not (descriptor:parent d))) (view:retire! actor id (model:revision id))))
                       (raise ex)])
            (place-document! actor manager rows w document #t id)))
        (place-document! actor manager rows w document #t))))

  (edoc "Return an active app to its saved document origin, or the most recent surviving retained document. Return false without changing placement when no target survives. Refuse if the invoking app is no longer active. Hidden app state survives, and unrelated panes and prompt focus stay unchanged."
        (actor actor "head") (manager model "manager") (window model "window") (app model "expected active app") (returns (or model #f)))
  (define (return! actor manager window app)
    (let* ([rows (capture actor manager)] [w (require-window rows manager window)]
           [ps (option w 'presentations '())] [origin (assoc app (option w 'origins '()))])
      (define (survives? p)
        (and p (not (equal? (car p) app))
          (if (handle:buffer? (car p)) (available-document? actor (car p)) (view:snapshot (car p)))))
      (unless (and (model:reference? app) (equal? app (active w)) (assoc app ps))
        (error 'return! "app is no longer active in this window" app))
      (let* ([previous (and origin (assoc (cadr origin) ps))]
             [target (or (and (survives? previous) previous) (find survives? ps))])
        (and target (place-document! actor manager rows w (car target) #f)))))

  (edoc "Select a window and remember it atomically with the containing root's focus."
        (actor actor "head") (manager model "manager view") (window model "window view"))
  (define (select! actor manager window)
    (let* ([rows (capture actor manager)] [m (record rows manager)])
      (let ([w (require-window rows manager window)])
        (configure! actor rows manager (with-option m 'selected window) (or (active w) window)))))

  (define (split-empty! actor manager window direction)
    (unless (memq direction '(left right above below)) (error 'split! "invalid direction"))
    (let* ([rows (capture actor manager)] [w (require-window rows manager window)]
           [parent (record rows (descriptor:parent (value w)))]
           [used (map (lambda (id) (option (record rows id) 'number #f)) (window-ids rows manager))]
           [number (let next ([n 1]) (if (memv n used) (next (+ n 1)) n))]
           [owner (descriptor:owner (value w))]
           [ids (model:allocate! actor 3
                  (lambda (ids)
                    (let ([parts (if (memq direction '(left above)) (list (car ids) window) (list window (car ids)))])
                      (map (lambda (d scope) (spec scope (descriptor:with d
                                                           (list (cons 'owner owner) (cons 'generation (if owner 1 0))))))
                        (list
                          (let ([d (window-descriptor (cadr ids) number (caddr ids))])
                            (descriptor:with d (list (cons 'options (append (display-options w) (descriptor:options d))))))
                          (descriptor:with (descriptor:make #f 'window-split 2
                                             (list (cons 'axis (if (memq direction '(left right)) 'x 'y))) '(1 1))
                            (list (cons 'parent (field parent 'id))
                              (cons 'children (map (lambda (key id) (list key id '(grow 1))) '(first second) parts))))
                          (status-descriptor (car ids))) (list manager manager (car ids)))))
                  (lambda (ids)
                    ;; Allocation and parent replacement share one commit. Keep
                    ;; every captured descriptor as a witness, including labels
                    ;; in branches this split does not modify.
                    (map (lambda (r)
                           (let ([d (cond [(equal? (field r 'id) window)
                                           (descriptor:with (value r) (list (cons 'parent (cadr ids))))]
                                      [(equal? (field r 'id) (field parent 'id))
                                       (descriptor:with (value r)
                                         (list (cons 'children (cadr (replace-child parent window (cadr ids))))))]
                                      [else (value r)])])
                             (change r d))) rows)))])
      (unless ids (error 'split! "manager changed before allocation"))
      (car ids)))

  (edoc "Split beside an existing window, retaining selection and independently copying its current presentation and saved app-return chain. Other hidden history is not copied. Sources and processes remain shared. Failed preparation removes the new pane and its copies. Logical orientation and proportions have no display units."
        (actor actor "head") (manager model "manager view") (window model "existing window")
        (direction (one-of left right above below) "new window's side") (returns model))
  (define (split! actor manager window direction)
    (let* ([rows (capture actor manager)] [w (require-window rows manager window)]
           [ps (option w 'presentations '())] [origins (option w 'origins '())]
           [chain
            (let walk ([document (active-document w)] [seen '()])
              (let ([p (and document (not (member document seen)) (assoc document ps))])
                (if (not p) '()
                  (let ([origin (assoc document origins)])
                    (append (if origin (walk (cadr origin) (cons document seen)) '()) (list p))))))]
           [created (split-empty! actor manager window direction)])
      (guard (ex [else (close! actor manager created) (raise ex)])
        (let ([mapping '()])
          (for-each
            (lambda (p)
              (let* ([tree (view:tree (cadr p))]
                     [rewire? (exists (lambda (row) (member window (map cadr (descriptor:commands (cdr row))))) tree)]
                     [copy (view:fork! actor (cadr p)
                             (append (list (cons 'owner created))
                               (if rewire? (list (list 'receivers (list window created))) '())))]
                     [identity (if (handle:buffer? (car p)) (car p) copy)]
                     [current (capture actor manager)])
                (unless (and (equal? (active-document w) (active-document (require-window current manager window)))
                          (or (not (handle:buffer? identity)) (available-document? actor identity)))
                  (error 'split! "source presentation changed during copy"))
                (place-document! actor manager current (require-window current manager created) identity #f copy)
                (set! mapping (cons (cons (car p) identity) mapping)))) chain)
          (unless (null? mapping)
            (let* ([rows (capture actor manager)] [target (require-window rows manager created)]
                   [origins (filter values
                              (map (lambda (p)
                                     (let ([app (assoc (car p) mapping)] [origin (assoc (cadr p) mapping)])
                                       (and app (list (cdr app) (and origin (cdr origin)))))) origins))])
              (configure! actor rows created (with-option target 'origins origins) #f))))
        created)))

  (define (cleanup-options r outputs)
    (if (null? outputs) (options r)
      (with-option r 'cleanup
        (fold-left (lambda (out id) (if (member id out) out (cons id out))) (descriptor:cleanup (value r)) outputs))))
  (define (finish-disposal! actor manager)
    (let retry ()
      (let ([r (model:snapshot manager)])
        (when (and (kind? r 'window-manager) (pair? (descriptor:cleanup (value r))))
          (let-values ([(status rows)
                        (view:finish-disposal! actor (descriptor:cleanup (value r))
                          (list (change r (descriptor:with (value r)
                                            (list (cons 'options (remp (lambda (p) (eq? (car p) 'cleanup)) (options r))))))))])
            (case status [(stale) (retry)] [(applied) (void)]
              [else (error 'close! "pending window disposal is unavailable" manager)]))))))

  (edoc "Close a window and collapse its split atomically with retirement of its owned and scoped model graph. Borrowed contents become unowned roots and borrowed sources survive. Persist output cleanup on the surviving manager before deletion; interrupted cleanup resumes at startup. The last window refuses."
        (actor actor "head") (manager model "manager view") (window model "window view") (returns boolean))
  (define (close! actor manager window)
    (let* ([rows (capture actor manager)] [w (require-window rows manager window)]
           [ids (window-ids rows manager)] [remaining (remove window ids)])
      (and (pair? remaining)
        (let* ([plan (view:disposal window #f)]
               [views (filter (lambda (r) (eq? (field r 'kind) 'widget-view)) (car plan))]
               [rows (with-retained rows views)]
               [m (record rows manager)] [split (record rows (descriptor:parent (value w)))]
               [split-id (field split 'id)] [parent (record rows (descriptor:parent (value split)))]
               [sibling (car (remove window (child-ids split)))] [current (selected rows manager)]
               [next (if (equal? current window) (car remaining) current)]
               [manager-options (cons (cons 'selected next)
                                  (cons (cons 'links (filter (lambda (l) (not (member window (list-head l 2)))) (option m 'links '())))
                                    (remp (lambda (p) (memq (car p) '(selected links))) (cleanup-options m (cadr plan)))))]
               [replaced (replace-child parent split-id sibling)]
               [updates (cons* (replacement split '() (options split)) replaced
                          (map (lambda (r) (replacement r '() (remp (lambda (p) (memq (car p) '(owned presentations cleanup))) (options r)))) views))])
          (when (exists (lambda (r) (member (field r 'id) (list manager split-id (field parent 'id) sibling))) (car plan))
            (error 'close! "window ownership reaches its surviving topology"))
          (commit! actor rows
            (if (equal? manager (field parent 'id))
              (cons (list manager (cadr replaced) manager-options) (remp (lambda (p) (equal? manager (car p))) updates))
              (cons (replacement m (children m) manager-options) updates))
            (and (equal? (focused rows manager) window) (list manager (or (active (record rows next)) next)))
            (cons (list split-id (field split 'revision)) (map (lambda (r) (list (field r 'id) (field r 'revision))) (car plan))))
          (finish-disposal! actor manager)
          #t))))

  (edoc "Set logical split weights only if its two children still match the displayed divider. No terminal sizes are persisted."
        (actor actor "head") (manager model "manager view") (split model "split view")
        (expected list "two child references, in order") (weights list "two positive rational weights"))
  (define (resize! actor manager split expected weights)
    (unless (and (list? weights) (= (length weights) 2) (for-all weight? weights)) (error 'resize! "invalid weights"))
    (let* ([rows (capture actor manager)] [r (record rows split)])
      (unless (and (kind? r 'window-split) (equal? manager (field r 'scope)) (equal? expected (child-ids r)))
        (error 'resize! "split target changed"))
      (let-values ([(status records)
                    (model:commit! actor
                      (map (lambda (row)
                             (change row (if (eq? row r)
                                           (fence (descriptor:with (value row) (list (cons 'state weights))))
                                           (value row)))) rows))])
        (unless (eq? status 'applied) (error 'resize! "split changed; retry" status)))))

  (edoc "Read directed (from to tag) links in creation order." (actor actor "head") (manager model "manager view") (returns list))
  (define (links actor manager) (option (record (capture actor manager) manager) 'links '()))
  (define (change-link! actor manager from to tag add?)
    (unless (and (symbol? tag) (not (equal? from to))) (error 'manager "expected a tag and distinct windows"))
    (let* ([rows (capture actor manager)] [m (record rows manager)] [link (list from to tag)] [links (option m 'links '())])
      (require-window rows manager from) (require-window rows manager to)
      (let ([next (if add? (if (member link links) links (append links (list link))) (remove link links))])
        (unless (equal? next links)
          (configure! actor rows manager (with-option m 'links next) #f)))))

  (edoc "Add an idempotent directed link between explicit windows." (actor actor "head") (manager model "manager view")
        (from model "source window") (to model "target window") (tag symbol "semantic tag"))
  (define (link! actor manager from to tag) (change-link! actor manager from to tag #t))

  (edoc "Remove one directed window link." (actor actor "head") (manager model "manager view")
        (from model "source window") (to model "target window") (tag symbol "semantic tag"))
  (define (unlink! actor manager from to tag) (change-link! actor manager from to tag #f))

  (define (reconcile! actor manager)
    (let retry ()
      (let ([generation (car (model:snapshots '()))])
        (guard (ex [else (if (= generation (car (model:snapshots '()))) (raise ex) (retry))])
          (let* ([rows (capture actor manager)]
                 [saved (map (lambda (id) (let ([w (record rows id)]) (cons w (retained rows w)))) (window-ids rows manager))]
                 [texts (apply append (map (lambda (entry) (filter (lambda (r) (handle:buffer? (document-of (car entry) r))) (cdr entry))) saved))]
                 [rows (fold-left (lambda (rows entry)
                                    (fold-left (lambda (rows r)
                                                 (with-retained rows (presentation-tree (field r 'id)))) rows (cdr entry))) rows saved)]
                 [facts (cadr (store:metadata (map (lambda (r) (descriptor:source (value r))) texts)))]
                 [missing-apps (apply append
                                 (map (lambda (entry)
                                        (filter (lambda (p) (not (model:snapshot (cadr p))))
                                          (option (car entry) 'presentations '()))) saved))]
                 [dead-texts (filter (lambda (r) (not (live-document? actor (cadr (assoc (descriptor:source (value r)) facts))))) texts)]
                 [plans (map view:disposal (append (map cadr missing-apps) (map (lambda (r) (field r 'id)) dead-texts)))]
                 [retired (fold-left (lambda (rs plan) (with-retained rs (car plan))) '() plans)]
                 [outputs (apply append (map cadr plans))]
                 [rows (with-retained rows (filter (lambda (r) (eq? (field r 'kind) 'widget-view)) retired))]
                 [removed (map (lambda (r) (field r 'id)) retired)]
                 [next
                  (fold-left
                    (lambda (rows entry)
                      (let* ([w (record rows (field (car entry) 'id))]
                             [ps (map (lambda (r) (list (document-of w r) (field r 'id)))
                                   (filter (lambda (r) (not (member (field r 'id) removed))) (cdr entry)))]
                             [old (active w)]
                             [target (if (and old (or (member old (map cadr ps))
                                                    (not (member old (map cadr (option w 'presentations '())))))) old
                                       (and (pair? ps) (cadar ps)))])
                        (if (and (equal? ps (option w 'presentations '())) (equal? old target)) rows
                          (map (lambda (r c)
                                 (map (lambda (p) (if (eq? (car p) 'value) (cons 'value (cadddr c)) p)) r)) rows
                            (placement rows w ps target (placement-focus? rows manager (field w 'id)) #f))))) rows saved)]
                 [changes (map (lambda (before after)
                                 (change before (if (equal? manager (field after 'id))
                                                  (descriptor:with (value after) (list (cons 'options (cleanup-options after outputs))))
                                                  (value after)))) rows next)])
            ;; A retired app's scope is authoritative even when its root was
            ;; removed directly. Never consume a descendant mounted elsewhere.
            (for-each (lambda (r)
                        (when (eq? (field r 'kind) 'widget-view)
                          (let ([d (value r)])
                            (unless (and (or (not (descriptor:owner d)) (equal? actor (descriptor:owner d)))
                                      (or (not (descriptor:parent d))
                                        (member (descriptor:parent d) removed)
                                        (assoc (descriptor:parent d) missing-apps)
                                        (member r texts)))
                              (error 'manager "retired app descendant belongs elsewhere"))))) retired)
            (unless (and (null? retired) (equal? rows next))
              (let-values ([(status ignored)
                            (model:retire-many! actor
                              (map (lambda (r) (list (field r 'id) (field r 'revision))) retired)
                              (filter (lambda (c) (not (member (car c) removed))) changes))])
                (case status
                  [(applied) (finish-disposal! actor manager)]
                  [(stale) (unless (= generation (car (model:snapshots '()))) (retry))]))))))))

  (define (reconcile-affected! references)
    ;; Retirement is infrequent. Read canonical windows instead of keeping a
    ;; second inventory; ordinary edits, frame publications and renames do not
    ;; enter this scan. Each manager's fallback is one guarded transaction.
    (let ([seen '()])
      (for-each
        (lambda (r)
          (when (and r (kind? r 'window)
                  (exists (lambda (p) (or (not references) (exists (lambda (id) (member id references)) p)))
                    (option r 'presentations '())))
            (let* ([id (field r 'scope)] [m (and (model:reference? id) (model:snapshot id))])
              (when (and m (kind? m 'window-manager) (not (member id seen)))
                (set! seen (cons id seen))
                ;; Unknown or externally malformed compositions stay inert;
                ;; they must not prevent other managers from reconciling.
                (guard (ex [else (void)]) (reconcile! (option m 'head #f) id))))))
        (map caddr (cadr (model:snapshots (model:ids 'widget-view)))))))

  (define lifecycle
    (kernel:call-with-runtime-registrations
      (lambda ()
        (list
          (store:subscribe! #f
            (lambda (event)
              (when (or (eq? (car event) 'delete)
                      (and (eq? (car event) 'property) (memq (caddr event) '(internal trashed backup audience))))
                (reconcile-affected! (list (cadr event))))))
          (model:subscribe! #f
            (lambda (event)
              (let ([ids (cadr event)])
                (if (not ids) (reconcile-affected! #f)
                  (let* ([alive (map car (model:metadata ids))]
                         [missing (filter (lambda (id) (not (member id alive))) ids)])
                    (unless (null? missing) (reconcile-affected! missing)))))))))))

  (edoc "Upgrade saved manager schema 1 by moving head identity out of its lifetime scope. Other view kinds and unknown versions stay unchanged."
        (r list "model envelope") (returns list))
  (define (upgrade r)
    (if (and (eq? (field r 'kind) 'widget-view) (= (field r 'schema) 3)
          (descriptor:valid? (value r)) (= (descriptor:schema (value r)) 1))
      (case (descriptor:kind (value r))
        [(window-manager)
         (if (descriptor:head? (field r 'scope))
           (let ([d (descriptor:with (value r) (list '(schema . 2) (cons 'options (with-option r 'head (field r 'scope)))))])
             (map (lambda (p) (case (car p) [(scope) '(scope . session)] [(value) (cons 'value d)] [else p])) r)) r)]
        [(window-split)
         (let ([d (descriptor:with (value r)
                    (list '(schema . 2) (cons 'state (map (lambda (c) (cadr (caddr c))) (children r)))
                      (cons 'children (map (lambda (c) (list (car c) (cadr c) '(grow 1))) (children r)))))])
           (map (lambda (p) (if (eq? (car p) 'value) (cons 'value d) p)) r))]
        [else r]) r))
)
