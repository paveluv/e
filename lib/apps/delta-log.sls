;; Independent review compositions. Choices and derivation belong to the base.
(import (only (foundation edoc) elibrary))
(elibrary (apps delta-log)
  (export choose! choose-all! conflicts conflicts! create! filter! init! log open! resolve! settle! show!)
  (import (except (chezscheme) log) (prefix (foundation edoc) edoc:) (prefix (foundation string) string:) (prefix (foundation text) text:)
          (prefix (head edit) edit:) (prefix (head editor) editor:) (prefix (head head) head:) (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:) (prefix (head layout) layout:) (prefix (head paint) paint:)
          (prefix (head table) table:) (prefix (head widget) widget:) (prefix (head window) window:)
          (prefix (service conflict-review) conflict-review:) (prefix (service conflict-source) conflict-source:)
          (prefix (service log) log:) (prefix (service review-preview) review-preview:)
          (prefix (service rewrite) rewrite:) (prefix (service rewrite-source) rewrite-source:)
          (prefix (state collection) collection:) (prefix (state connection) connection:)
          (prefix (state store) store:) (prefix (state view) view:) (prefix (sys glyph) glyph:))
  (define (get r k fallback) (cond [(and r (assq k r)) => cdr] [else fallback]))
  (define (child id name) (cadr (assq name (view:children (interaction:snapshot id)))))
  (define (query id) (view:source (interaction:snapshot id)))
  (define (kind id) (get (view:options (interaction:snapshot id)) 'review #f))
  (define (current-document)
    (or (head:buffer-store-id (head:current-buffer)) (error 'delta-log "current buffer has no shared document")))
  (define (selector v)
    (if (or (null? v) (and (pair? v) (pair? (car v)) (memq (caar v) '(count actor batch since until state))))
      v (list (cons 'batch (edoc:type-value 'batch v)))))
  (define (hint e)
    (let ([d (list-ref e 3)])
      (format "~s  ~a:~a  -~s  +~s" (cadr e) (caar d) (cadar d)
        (string:elide (string:join (cadr d) "\n") 24) (string:elide (string:join (caddr d) "\n") 24))))
  ;; The remaining type-preview consumer moves with prompt presentations.
  (define revision-mark #f)
  (define (preview-revision! revision)
    (let* ([b (head:current-buffer)] [id (head:buffer-store-id b)]
           [entry (and id (store:revision-span id revision))])
      (and entry (cadr entry)
        (let ([point (head:point)] [mark (cons b (text:datum->span (cadr entry)))])
          (set! revision-mark mark) (head:goto! (text:span-start (cdr mark)))
          (lambda ()
            (when (eq? revision-mark mark)
              (set! revision-mark #f)
              (when (memq b (head:buffers)) (head:with-buffer b (head:goto! point)))))))))
  (define (revision-highlights)
    (if (not revision-mark) '()
      (let* ([b (car revision-mark)] [span (cdr revision-mark)] [start (text:span-start span)] [end (text:span-end span)])
        (let loop ([row (car start)] [out '()])
          (if (> row (car end)) (reverse out)
            (loop (+ row 1) (cons (list b row (if (= row (car start)) (cdr start) 0)
                                    (if (= row (car end)) (cdr end) (string-length (head:buffer-line b row))) 'match) out)))))))

  (edoc-type revision "a retained revision of the current document"
    (predicate (lambda (v) (and (integer? v) (exact? v) (> v 0))))
    (complete (lambda (partial) (guard (ex [else '()]) (map (lambda (e) (cons (car e) (hint e))) (store:log (current-document))))))
    (write number->string) (preview preview-revision!))
  (edoc-type batch "the batch label of edits made together in the current document"
    (predicate pair?)
    (complete (lambda (partial)
                (guard (ex [else '()])
                  (reverse (fold-left (lambda (out e)
                                        (let ([b (get (caddr e) 'batch #f)])
                                          (if (or (not b) (assoc b out)) out (cons (cons b (format "Edits by ~s" (cadr e))) out))))
                             '() (store:log (current-document)))))))
    (write (lambda (v) (format "'~s" v))))
  (edoc-type conflict "a pending reload conflict in the current document"
    (predicate (lambda (v) (and (integer? v) (exact? v) (> v 0))))
    (complete (lambda (partial)
                (guard (ex [else '()])
                  (map (lambda (c) (cons (car c) (format "mine ~s; disk ~s" (string:join (list-ref c 4) "\n")
                                                         (string:join (list-ref c 5) "\n")))) (store:conflicts (current-document))))))
    (write number->string))

  (edoc "Read the current document's retained history, newest first. Select by count, actor, batch, since, until or state; a batch literal selects its entries."
        (options (list-of (or list batch)) "optional selector") (returns list) (public))
  (define (log . options) (apply store:log (current-document) (map selector options)))

  (edoc "Describe a retained entry of the current document in the echo area."
        (revision revision "entry") (public))
  (define (show! revision)
    (let* ([n (edoc:type-value 'revision revision)] [e (assv n (store:log (current-document)))])
      (unless e (error 'show! "entry is no longer retained")) (edit:set-message! (format "~a  ~a" n (hint e)))))

  (edoc "Read the current document's pending reload conflicts as (revision actor labels region mine disk) records."
        (returns list) (public))
  (define (conflicts) (store:conflicts (current-document)))

  (edoc "Settle one current-document conflict with Mine, Disk or replacement lines through the store's undoable resolution."
        (conflict conflict "pending entry") (choice (or (one-of disk mine) (list-of string)) "side or replacement")
        (returns symbol) (public))
  (define (resolve! conflict choice)
    (let-values ([(status detail) (store:resolve! head:ui-actor (current-document) (edoc:type-value 'conflict conflict) choice 'any)])
      (head:before-frame!) (log:add! 'delta-log:resolve! (format "~a ~s" status detail)) status))

  (edoc "Create an unmounted conflict or rewrite review with an independent draft, bounded table and read-only preview. The ordered document scope is explicit; rewrite accepts exactly one document. Commands contain the host's return target. Removing a view preserves its draft; retiring the query removes its owned draft and output."
        (commands list "host commands") (kind (one-of conflicts rewrite) "review kind")
        (documents (list-of integer) "borrowed source documents") (returns model) (public))
  (define (create! commands kind documents)
    (unless (and (memq kind '(conflicts rewrite)) (list? documents)
              (or (eq? kind 'conflicts) (= (length documents) 1))) (error 'create! "invalid review scope"))
    (let* ([draft (if (eq? kind 'conflicts) (conflict-review:create! head:ui-actor documents) (rewrite:create! head:ui-actor (car documents)))]
           [p (review-preview:create! head:ui-actor draft)]
           [q (collection:create! head:ui-actor draft "" '() 'persistent (list draft (car p) (list 'buffer (cadr p))))]
           [root (view:create! head:ui-actor q 'delta-review 1 (list (cons 'commands commands) (cons 'review kind)) '() q)]
           [table (table:create! head:ui-actor q
                    (if (eq? kind 'conflicts) '(buffer revision actor position mine disk) '(revision actor position removed inserted state choice))
                    (append '((presentation review 1))
                      (if (eq? kind 'conflicts) '((identity . buffer) (cell-commands (mine . mine) (disk . disk))) '((identity . inserted)))))]
           [heading (view:create! head:ui-actor #f 'row 1 '((spacing . normal)) '() q)]
           [preview (view:create! head:ui-actor (car p) 'review-preview-panel 1 '() '() q)]
           [status (view:create! head:ui-actor (car p) 'review-status 1 '() '() q)]
           [editor (view:create! head:ui-actor (list 'buffer (cadr p)) 'editor 1
                     '((read-only . #t) (wrap . #f) (annotations)) '((0 . 0) (0 . 0) (0 . 0) #f) q)]
           [d (view:snapshot table)]
           [button (lambda (label action args)
                     (view:create! head:ui-actor #f 'action-text 1
                       (list (cons 'text label) '(enabled . #t) (list 'commands (list 'activate root action args))) '() q))]
           [buttons (append (if (eq? kind 'conflicts)
                              (list (list 'mine (button "Mine (all)" 'choose-all '(mine)) 'fit)
                                (list 'disk (button "Disk (all)" 'choose-all '(disk)) 'fit)) '())
                      (list (list 'settle (button "Settle" 'settle '()) 'fit)))]
           [table-commands (map (lambda (command) (list command root 'choose (list command)))
                             (if (eq? kind 'conflicts) '(activate mine disk) '(activate)))])
      (view:arrange! head:ui-actor
        (list (list root 0 (list (list 'heading heading 'fit) (list 'table table '(grow 1)) (list 'preview preview '(grow 1)))
                (list (cons 'commands commands) (cons 'review kind)))
          (list heading 0 buttons '((spacing . normal)))
          (list table 1 (view:children d) (cons (cons 'commands table-commands) (view:options d)))
          (list preview 0 (list (list 'status status 'fit) (list 'text editor '(grow 1))) '())) '())
      (for-each
        (lambda (binding)
          (let-values ([(status detail) (connection:bind! head:ui-actor (car binding) (cdr binding))])
            (unless (eq? status 'applied) (error 'create! "review connection refused" status detail))))
        (list (list (car p) (list (car p) 'selection #f (list table 'selection)))
          (list root (list editor 'annotations #f (list (car p) 'annotations))))) root))

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
  (define (busy? id d)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (eq? (get (get source 'value '()) 'status #f) 'pending)))
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
    (let* ([host (window:tool! name (lambda (commands) (create! commands kind documents)) (format "~a:~s" kind documents))])
      (window:show-widget! window host)
      (let ([app (child host 'app)]) (widget:focus! host (child (child (child app 'table) 'body) 'rows)) app)))

  (edoc "Open a retained rewrite review of the current document in the chosen window, current by default."
        (window (list-of window) "optional target window") (returns model))
  (define (open! . window)
    (show-review! "delta-log" 'rewrite (list (current-document))
      (if (null? window) (head:current-window) (edoc:type-value 'window (car window)))))

  (edoc "Open a retained conflict review. Capture visible shared documents once, with the invoking document first; nested hosts pass their scope to create! directly."
        (window (list-of window) "optional target window") (returns model))
  (define (conflicts! . window)
    (let ([documents (fold-left (lambda (out w)
                                  (let ([id (head:buffer-store-id (head:window-buffer w))])
                                    (if (or (not id) (memv id out)) out (append out (list id)))))
                       (let ([id (head:buffer-store-id (head:current-buffer))]) (if id (list id) '())) (head:windows))])
      (show-review! "conflicts" 'conflicts documents
        (if (null? window) (head:current-window) (edoc:type-value 'window (car window))))))

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
    (keymap:bind-default! "C-x l" (keymap:call open! 0))
    (keymap:bind-default! "C-x !" (keymap:call conflicts! 0))
    (paint:add-highlighter! revision-highlights)
    (paint:set-conflicts-action! (lambda () (conflicts! 0)))))
