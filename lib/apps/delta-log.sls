;; Independent review compositions. Choices and derivation belong to the base.
(import (only (foundation edoc) elibrary))
(elibrary (apps delta-log)
  (export choose! choose-all! conflicts conflicts! create! filter! init! log open! resolve! settle! show!)
  (import (except (chezscheme) log)
          (prefix (core handle) handle:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)

          (prefix (head editor) editor:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:)
          (prefix (head layout) layout:)
          (prefix (head prompt) prompt:)
          (prefix (head table) table:)
          (prefix (head widget) widget:)
          (prefix (head window-control) window-control:)
          (prefix (service change-preview) change-preview:)
          (prefix (service conflict-review) conflict-review:)
          (prefix (service conflict-source) conflict-source:)
          (prefix (service log) log:)
          (prefix (service review-preview) review-preview:)
          (prefix (service rewrite) rewrite:)
          (prefix (service rewrite-source) rewrite-source:)
          (prefix (service window) window:)
          (prefix (state collection) collection:)
          (prefix (state connection) connection:)
          (prefix (state construction) construction:)
          (prefix (state model) model:)
          (prefix (state store) store:)
          (prefix (state view) view:)
          (prefix (sys glyph) glyph:))

  (define (get r k fallback) (cond [(and r (assq k r)) => cdr] [else fallback]))
  (define (child id name) (cadr (assq name (view:children (interaction:snapshot id)))))
  (define (query id) (view:source (interaction:snapshot id)))
  (define (kind id) (get (view:options (interaction:snapshot id)) 'review #f))
  (define (completion-document)
    (let ([d (and (widget:target) (interaction:snapshot (widget:target)))])
      (and d (eq? (view:kind d) 'editor) (view:source d))))
  (define (selector v)
    (if (or (null? v) (and (pair? v) (pair? (car v)) (memq (caar v) '(count actor batch since until state))))
      v (list (cons 'batch (edoc:type-value 'batch v)))))
  (define (hint e)
    (let ([d (list-ref e 3)])
      (format "~s  ~a:~a  -~s  +~s" (cadr e) (caar d) (cadar d)
        (string:elide (string:join (cadr d) "\n") 24) (string:elide (string:join (caddr d) "\n") 24))))
  (define (make-change-preview request context commands)
    (let* ([argument (get context 'argument '())] [document (get argument 'document #f)])
      (if (not document) (prompt:create-choices! request commands 'table request)
        (let* ([root (view:create! head:ui-actor request 'change-preview 1 '() '(#f #f) request)]
               [query (change-preview:create! head:ui-actor document root)]
               [choices (prompt:create-choices! request commands 'table root)]
               [editor (editor:create-view! head:ui-actor document '((read-only . #t)) root)]
               [status (view:create! head:ui-actor query 'review-status 1 '() '() root)])
          (view:arrange! head:ui-actor
            (list (list root 0 (list (list 'choices choices 'fit) (list 'text editor '(grow 1)) (list 'status status 'fit))
                    (list (cons 'query query) (cons 'type (get argument 'type #f)) (list 'owned query)))) '())
          (connection:bind! head:ui-actor query (list (list query 'selection #f (list root 'selection))))
          (connection:bind! head:ui-actor root (list (list editor 'annotations #f (list query 'annotations)))) root))))
  (define (change-service! id frame)
    (let* ([d (interaction:snapshot id)] [request (model:snapshot (view:source d))]
           [argument (get (prompt:completion-context (view:source d)) 'argument '())]
           [type (get (view:options d) 'type #f)]
           [value (get argument 'value #f)]
           [selection (and (eq? type (get argument 'type #f)) (integer? value) (exact? value) (> value 0) (list type value))])
      (when (and request (eq? (get (get request 'value '()) 'status #f) 'editing))
        (unless (equal? selection (car (view:state d)))
          (interaction:set-state! head:ui-actor id #f (list selection #f)))
        (let* ([record (model:snapshot (get (view:options d) 'query #f))] [v (get record 'value '())]
               [basis (get v 'basis #f)] [annotations (get v 'annotations '())] [editor (child id 'text)])
          (when (and selection basis (equal? selection (cadar basis)) (eq? (get v 'status #f) 'ready)
                  (pair? annotations) (= (cadr annotations) (caddr (editor:basis editor)))
                  (not (equal? basis (cadr (view:state (interaction:snapshot id))))))
            (let ([span (caaddr annotations)])
              (when span (editor:move! editor (cons (caar span) (cadar span)))))
            (interaction:set-state! head:ui-actor id #f (list selection basis)))))))

  (edoc-type revision "a retained revision of the captured editor's document"
    (predicate (lambda (v) (and (integer? v) (exact? v) (> v 0))))
    (complete (lambda (partial) (guard (ex [else '()]) (map (lambda (e) (list (car e) #f (hint e))) (store:log (completion-document)))))))
  (edoc-type batch "the batch label of edits made together in the current document"
    (predicate pair?)
    (complete (lambda (partial)
                (guard (ex [else '()])
                  (reverse (fold-left (lambda (out e)
                                        (let ([b (get (caddr e) 'batch #f)])
                                          (if (or (not b) (assoc b out)) out (cons (list b #f (format "Edits by ~s" (cadr e))) out))))
                             '() (store:log (completion-document))))))))
  (edoc-type conflict "a pending reload conflict in the current document"
    (predicate (lambda (v) (and (integer? v) (exact? v) (> v 0))))
    (complete (lambda (partial)
                (guard (ex [else '()])
                  (map (lambda (c) (list (car c) #f (format "mine ~s; disk ~s" (string:join (list-ref c 4) "\n")
                                                      (string:join (list-ref c 5) "\n")))) (store:conflicts (completion-document)))))))

  (edoc "Read a document's retained history, newest first. Select by count, actor, batch, since, until or state; a batch literal selects its entries."
        (document buffer "source document") (options (list-of (or list batch)) "optional selector") (returns list) (public))
  (define (log document . options) (apply store:log document (map selector options)))

  (edoc "Describe a retained entry of an editor's document in the journal."
        (receiver editor (view editor)) (editor model "source editor") (revision revision "entry") (public))
  (define (show! editor revision)
    (let* ([n (edoc:type-value 'revision revision)] [e (assv n (store:log (cadr (editor:basis editor))))])
      (unless e (error 'show! "entry is no longer retained")) (log:add! 'delta-log:show! (format "~a  ~a" n (hint e)))))

  (edoc "Read a document's pending reload conflicts as (revision actor labels region mine disk) records."
        (document buffer "source document") (returns list) (public))
  (define (conflicts document) (store:conflicts document))

  (edoc "Settle one document conflict with Mine, Disk or replacement lines through the store's undoable resolution."
        (document buffer "source document") (conflict conflict "pending entry") (choice (or (one-of disk mine) (list-of string)) "side or replacement")
        (returns symbol) (public))
  (define (resolve! document conflict choice)
    (let-values ([(status detail) (store:resolve! head:ui-actor document (edoc:type-value 'conflict conflict) choice 'any)])
      (head:before-frame!) (log:add! 'delta-log:resolve! (format "~a ~s" status detail)) status))

  (edoc "Create an unmounted conflict or rewrite review with an independent draft, bounded table and read-only preview. The ordered document scope is explicit; rewrite accepts exactly one document. Commands contain the host's return target. Retiring the app removes its private preview and output, preserving the borrowed query and draft; retiring the query removes its owned draft."
        (owner (or model #f) "lifetime owner, false for a session root") (commands list "host commands") (kind (one-of conflicts rewrite) "review kind")
        (documents (list-of buffer) "borrowed source documents") (returns model) (public))
  (define (create! owner commands kind documents)
    (construction:call! head:ui-actor
      (lambda (remember!)
        (unless (and (memq kind '(conflicts rewrite)) (list? documents)
                  (or (eq? kind 'conflicts) (= (length documents) 1))) (error 'create! "invalid review scope"))
        (let* ([draft (if (eq? kind 'conflicts) (remember! (conflict-review:create! head:ui-actor documents)) (remember! (rewrite:create! head:ui-actor (car documents))))]
               [q (remember! (collection:create! head:ui-actor draft "" '() 'persistent (list draft)))]
               [root (remember! (view:create! head:ui-actor q 'delta-review 1 (list (cons 'commands commands) (cons 'review kind)) '() owner))]
               [p (review-preview:create! head:ui-actor draft root)]
               [table (table:create! head:ui-actor root q
                        (if (eq? kind 'conflicts) '(buffer revision actor position mine disk) '(revision actor position removed inserted state choice))
                        (append '((presentation review 1))
                          (if (eq? kind 'conflicts) '((identity . buffer) (cell-commands (mine . mine) (disk . disk))) '((identity . inserted)))))]
               [heading (remember! (view:create! head:ui-actor #f 'row 1 '((spacing . normal)) '() root))]
               [preview (remember! (view:create! head:ui-actor (car p) 'review-preview-panel 1 '() '() root))]
               [status (remember! (view:create! head:ui-actor (car p) 'review-status 1 '() '() root))]
               [editor (remember! (view:create! head:ui-actor (cadr p) 'editor 1
                                    '((read-only . #t) (wrap . #f) (annotations)) '((0 . 0) (0 . 0) (0 . 0) #f) root))]
               [d (view:snapshot table)]
               [button (lambda (label action args)
                         (remember! (view:create! head:ui-actor #f 'action-text 1
                                      (list (cons 'text label) '(enabled . #t) (list 'commands (list 'activate root action args))) '() root)))]
               [buttons (append (if (eq? kind 'conflicts)
                                  (list (list 'mine (button "Mine (all)" 'choose-all '(mine)) 'fit)
                                    (list 'disk (button "Disk (all)" 'choose-all '(disk)) 'fit)) '())
                          (list (list 'settle (button "Settle" 'settle '()) 'fit)))]
               [table-commands (map (lambda (command) (list command root 'choose (list command)))
                                 (if (eq? kind 'conflicts) '(activate mine disk) '(activate)))])
          (view:arrange! head:ui-actor
            (list (list root 0 (list (list 'heading heading 'fit) (list 'table table '(grow 1)) (list 'preview preview '(grow 1)))
                    (list (cons 'commands commands) (cons 'review kind) (list 'owned (car p))))
              (list heading 0 buttons '((spacing . normal)))
              (list table 1 (view:children d) (cons (cons 'commands table-commands) (view:options d)))
              (list preview 0 (list (list 'status status 'fit) (list 'text editor '(grow 1))) '())) '())
          (for-each
            (lambda (binding)
              (let-values ([(status detail) (connection:bind! head:ui-actor (car binding) (cdr binding))])
                (unless (eq? status 'applied) (error 'create! "review connection refused" status detail))))
            (list (list (car p) (list (car p) 'selection #f (list table 'selection)))
              (list root (list editor 'annotations #f (list (car p) 'annotations))))) root))))

  (define (validate-selection! id selection)
    (unless (and (list? selection) (= (length selection) 3) (equal? (car selection) (query id)))
      (error 'delta-log "selection belongs to another review")))

  (edoc "Choose Mine or Disk for the exact shown conflict; activate flips its choice. In rewrite reviews, activate toggles the shown revision. Neither action settles the draft."
        (receiver id (view delta-review)) (id model "review") (command (one-of activate mine disk) "row action")
        (selection row-selection "shown entry") (basis datum "shown result basis"))
  (define (choose! id command selection basis)
    (validate-selection! id selection)
    (if (eq? (kind id) 'rewrite)
      (begin (unless (eq? command 'activate) (error 'choose! "rewrite rows only toggle"))
        (rewrite-source:toggle! head:ui-actor selection basis))
      (conflict-source:choose! head:ui-actor selection basis (if (eq? command 'activate) 'flip command))))
  (define (shown id)
    (let* ([state (view:state (interaction:snapshot (child id 'table)))] [selection (get state 'selection #f)])
      (validate-selection! id selection) (list (car selection) (cadr selection) (get state 'basis '()))))

  (edoc "Choose one side throughout the shown conflict review. The displayed table generation and draft basis fence the entire choice; no source text changes."
        (receiver id (view delta-review)) (id model "review") (side (one-of mine disk) "bulk choice"))
  (define (choose-all! id side)
    (unless (eq? (kind id) 'conflicts) (error 'choose-all! "expected a conflict review"))
    (apply conflict-source:choose-all! head:ui-actor (append (shown id) (list side))))

  (edoc "Settle the exact shown review as one undoable change per source. Stale listings refuse; failed choices remain available."
        (receiver id (view delta-review)) (id model "review") (returns list))
  (define (settle! id)
    (let ([result (apply (if (eq? (kind id) 'conflicts) conflict-source:settle! rewrite-source:settle!) head:ui-actor (shown id))])
      (log:add! 'delta-log:settle! result) result))

  (edoc "Narrow a rewrite review by the store's history selector or a batch literal; false shows all retained entries. Existing draft choices survive filtering."
        (receiver id (view delta-review)) (id model "rewrite review") (value (or list batch #f) "selector") (public))
  (define (filter! id value)
    (unless (eq? (kind id) 'rewrite) (error 'filter! "expected a rewrite review"))
    (let ([r (collection:summary (query id))])
      (collection:configure! head:ui-actor (query id) (get r 'revision #f)
        (list (cons 'filter (if value (format "~s" (selector value)) ""))))))

  (define (preview-service! id frame)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (let* ([v (get source 'value '())] [basis (get v 'basis #f)]
             [selection (and basis (cadr basis))] [key (and selection (caddr selection))]
             [annotations (get v 'annotations '())] [editor (child id 'text)])
        (when (and key (memq (get v 'status #f) '(ready blocked)) (pair? annotations)
                (not (equal? key (view:state d))) (= (cadr annotations) (caddr (editor:basis editor))))
          (let ([selected (find (lambda (p) (memq (cadr p) '(match conflict-mine-current conflict-disk-current))) (caddr annotations))])
            (when selected (editor:move! editor (cons (caar selected) (cadar selected))))
            (interaction:set-state! head:ui-actor id #f key))))))
  (define (busy? source d)
    (eq? (get (get source 'value '()) 'status #f) 'pending))
  (define (status-text id source inputs)
    (let ([v (get source 'value '())])
      (cond [(eq? (get v 'status #f) 'unavailable) "[Preview unavailable; refresh the selection]"]
        [(eq? (get v 'status #f) 'blocked) "[Rewrite blocked by later edits]"]
        [(get v 'truncated? #f) "[Preview; additional regions unmarked]"] [else "Preview"])))
  (define (present cell cells attrs)
    (list (if (eq? (car cell) 'ready) (let ([v (cadr cell)]) (if (string? v) v (format "~s" v))) "")))
  (define (side name)
    (lambda (cell cells attrs)
      (let* ([text (car (present cell cells attrs))] [choice (assq 'choice cells)])
        (if (and choice (eq? (cadr choice) 'ready) (eq? (caddr choice) name))
          (list text (list 0 (string-length text) (if (eq? name 'mine) 'conflict-mine 'conflict-disk))) (list text)))))
  (define target-table (keymap:call widget:descendant widget:target 'table))
  (define (show-review! name kind documents window)
    (let ([app (window-control:open-app! window name (lambda (owner commands) (create! owner commands kind documents)) (format "~a:~s" kind documents))])
      (widget:pump!) (widget:focus! app (widget:descendant app 'table 'body 'rows)) app))

  (edoc "Open a retained rewrite review of this window's document. Nested compositions can pass their explicit document scope to create!."
        (receiver window (view window)) (window model "window") (returns model))
  (define (open! window)
    (let ([document (window:document (window-control:manager window) window)])
      (unless (handle:buffer? document) (error 'open! "this window does not show an editable document"))
      (show-review! "delta-log" 'rewrite (list document) window)))

  (edoc "Open a retained conflict review. Capture visible shared documents once, with the invoking document first; nested hosts pass their scope to create! directly."
        (receiver window (view window)) (window model "window") (returns model))
  (define (conflicts! window)
    (let* ([manager (window-control:manager window)]
           [documents (fold-left (lambda (out w)
                                   (let ([id (window:document manager w)])
                                     (if (and (handle:buffer? id) (not (member id out))) (append out (list id)) out)))
                        '() (cons window (remove window (window:list manager))))])
      (show-review! "conflicts" 'conflicts documents window)))

  (edoc "Register the review composition and inspectable table commands. No draft is created until requested." (public))
  (define (init!)
    (widget:register! 'delta-review 1
      (append (layout:container 'y) (list '(receivers (table table)) '(capture-contexts delta-review)
                                      (cons 'actions (list (cons 'choose choose!) (cons 'choose-all choose-all!) (cons 'settle settle!))))))
    (widget:register! 'review-preview-panel 1
      (append (layout:container 'y) (list (cons 'busy? busy?) (cons 'service preview-service!))))
    (widget:register! 'review-status 1
      (list (cons 'prepare status-text)
        (cons 'render (lambda (data d width height range) (if (zero? (car range)) (list (glyph:fit data width)) '())))
        (cons 'measure (lambda (data d axis cross child) (if (eq? axis 'y) '(1 1) (list 0 (glyph:cells data)))))
        (cons 'decorate (lambda (data d width height range) (list (list (list 0 0 (min width (glyph:cells data)) 1) 'ghost))))))
    (table:register-presentation! 'review 1
      (append (map (lambda (name) (list name (if (memq name '(removed inserted)) 12 5) 'text '() present))
                   '(buffer revision actor position removed inserted state choice))
        (list (list 'mine 12 'text '(choice) (side 'mine)) (list 'disk 12 'text '(choice) (side 'disk)))))
    (keymap:bind-default! 'delta-review "LEFT" (keymap:call table:invoke! target-table 'mine))
    (keymap:bind-default! 'delta-review "RIGHT" (keymap:call table:invoke! target-table 'disk))
    (keymap:bind-default! 'delta-review "SPC" (keymap:call table:invoke! target-table 'activate))
    (keymap:bind-default! 'delta-review "S-LEFT" (keymap:call choose-all! widget:target 'mine))
    (keymap:bind-default! 'delta-review "S-RIGHT" (keymap:call choose-all! widget:target 'disk))
    (keymap:bind-default! 'delta-review "M-RET" (keymap:call settle! widget:target))
    (keymap:bind-default! 'delta-review "ESC" (keymap:call widget:invoke! widget:target 'return))
    (keymap:bind-default! 'composed-window "C-x C-l" (keymap:call open! widget:target))
    (keymap:bind-default! 'composed-window "C-x !" (keymap:call conflicts! widget:target))
    (for-each (lambda (type) (prompt:register-presentation! type make-change-preview)) '(revision conflict))
    (widget:register! 'change-preview 1
      (append (remp (lambda (p) (eq? (car p) 'measure)) (layout:container 'y))
        (list (cons 'service change-service!)
          (cons 'measure (lambda (data d axis cross child) (if (eq? axis 'y) '(0 7) '(0 1)))))))
  ))
