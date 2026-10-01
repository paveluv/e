;; Prompt controls own no keyboard reader or window. The host places the tree.
(import (only (foundation edoc) elibrary))
(elibrary (head prompt)
  (export accept! active? cancel! choose! complete! completion-context create! create-choices! drain! edge! history! init! inspect! key! newline! read! register-host! register-presentation! register-profile!)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (foundation string) string:) (prefix (foundation text) text:)
          (prefix (head completion) completion:) (prefix (head completion-layout) completion-layout:)
          (prefix (head completion-state) completion-state:)
          (prefix (head editor) editor:)
          (prefix (head entry) entry:) (prefix (head head) head:) (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:) (prefix (head layout) layout:)
          (prefix (head suspension) suspension:) (prefix (head table) table:) (prefix (head text-control) text-control:) (prefix (head text-source) text-source:)
          (prefix (head widget) widget:) (prefix (service log) log:)
          (prefix (service prompt-request) prompt-request:) (prefix (state model) model:)
          (prefix (state view) view:) (prefix (sys glyph) glyph:))

  (define (get r key) (cdr (assq key r)))

  (edoc "Whether the focused widget path contains a prompt, without reading remote state."
        (returns boolean))
  (define (active?)
    (let ([root (head:window-widget (head:current-window))])
      (and root
        (let loop ([id (widget:focused root)])
          (let ([d (and id (interaction:snapshot id))])
            (and d (or (eq? (view:kind d) 'prompt) (and (view:parent d) (loop (view:parent d))))))))))
  (define live (make-hashtable equal-hash equal?))
  (define hovered (make-hashtable equal-hash equal?))
  (define hosts (kernel:make-registry))
  (define profiles (kernel:make-registry car))
  (define presentations (kernel:make-registry car))
  (define presentation-factories (make-hashtable equal-hash equal?))
  (define tickets (make-hashtable equal-hash equal?))
  (define host-changes
    (kernel:registry-observe! hosts
      (lambda (removed added)
        (let-values ([(ids entries) (hashtable-entries tickets)])
          (vector-for-each
            (lambda (entry) (when (memq (car entry) removed) (suspension:cancel! (cdr entry)))) entries)))))
  (define pending '())
  (define-record-type runtime (fields request (mutable generation) factory source completion profile-factory profile
                                (mutable signature) (mutable delivered?) (mutable closed?)
                                (mutable message) (mutable history-index) (mutable stash) (mutable edge) (mutable kind) (mutable context)))
  (define (option options key fallback) (cond [(assq key options) => cdr] [else fallback]))

  (edoc "Register an exact edoc argument type's completion presentation. The module-owned factory receives (request context commands) and returns an unmounted composition. Select passes generation and insertion text; cancel cancels the prompt. Different owners cannot claim the same type; missing types use the generic presentation."
        (type datum "exact resolved type, including compound type data")
        (factory procedure "request, explicit context and named select/cancel targets -> view") (public))
  (define (register-presentation! type factory)
    (unless (and type (procedure? factory)) (error 'register-presentation! "expected a type and factory"))
    (kernel:registry-add! presentations (cons type factory)))

  (edoc "Read a prompt's prepared completion snapshot and explicit argument context without querying the provider. Candidates and source handles are head data; draft, origin and request identity belong to the base."
        (request model "acquired prompt request") (returns list) (effects internal) (public))
  (define (completion-context request)
    (let* ([source (model:snapshot request)] [value (and source (get source 'value))]
           [r (and value (hashtable-ref live (get value 'controller) #f))])
      (list (cons 'request request) (cons 'origin (if value (get value 'origin) #f))
        (cons 'argument (if r (runtime-context r) '()))
        (cons 'snapshot (and r (runtime-completion r) (completion-state:snapshot (runtime-completion r)))))))

  (edoc "Create a completion choice control over the prompt's existing candidate set. Table and columns are head layouts; choosing only proposes literal insertion through the supplied select target."
        (request model "prompt request") (commands list "explicit select/cancel targets")
        (layout (one-of columns table) "candidate layout") (owner model "lifetime owner, usually request or containing view") (returns model) (public))
  (define (create-choices! request commands layout owner)
    (unless (memq layout '(columns table)) (error 'create-choices! "expected columns or table"))
    (view:create! head:ui-actor request 'prompt-choices 1 (list (cons 'commands commands) (cons 'layout layout)) '() owner))
  (define (default-presentation request context commands) (create-choices! request commands 'columns request))
  (define (table-presentation request context commands) (create-choices! request commands 'table request))

  (define (presentation-service! id frame)
    (let-values ([(source d inputs) (widget:context id)])
      (let* ([context (completion-context (view:source d))] [argument (get context 'argument)]
             [type (option argument 'type #f)] [registration (kernel:registry-find presentations (lambda (r) (equal? (car r) type)))]
             [factory (if registration (cdr registration) default-presentation)] [key (list (and registration type) factory)]
             [old (hashtable-ref presentation-factories id #f)])
        ;; Once accepted, a queued outcome owns the controller's lease.
        ;; Replacing a child now would renew it and strand that outcome.
        (unless (or (not (eq? (get (get source 'value) 'status) 'editing)) (equal? key old))
          (let ([next (factory (view:source d) context (get (view:options d) 'commands))]
                [prior (and (pair? (view:children d)) (cadar (view:children d)))])
            (let-values ([(status ignored) (widget:arrange! (list (list id (get (model:snapshot id) 'revision)
                                                                    (list (list 'presentation next 'fit)) (view:options d))))])
              (if (eq? status 'applied)
                (begin (hashtable-set! presentation-factories id key)
                  (when prior (let ([r (caddar (cadr (model:snapshots (list prior))))])
                                (when r (view:retire! head:ui-actor prior (get r 'revision))))))
                (let ([r (caddar (cadr (model:snapshots (list next))))])
                  (when r (view:retire! head:ui-actor next (get r 'revision)))))))))))
  (define (profile-factory d)
    (let ([recipe (option (view:options d) 'profile #f)])
      (and recipe (kernel:registry-find profiles (lambda (p) (equal? (car p) (list-head recipe 2)))))))

  (edoc "Register a prompt interaction profile factory, called once per mounted request with configuration and captured origin. It returns optional history strings, alternate completion and pure normalize/validate/ghost/transform/edge callbacks, plus an inspect command. Callbacks run in input/service work, never painting."
        (name symbol "profile namespace") (schema integer "positive version") (factory procedure "(configuration origin) -> profile alist"))
  (define (register-profile! name schema factory)
    (unless (and (symbol? name) (integer? schema) (exact? schema) (> schema 0) (procedure? factory))
      (error 'register-profile! "invalid profile registration"))
    (kernel:registry-add! profiles (cons (list name schema) factory)))
  (define (make-profile d origin)
    (let* ([recipe (option (view:options d) 'profile #f)] [factory (profile-factory d)]
           [profile (if factory ((cdr factory) (caddr recipe) origin) '())])
      (when (and recipe (not factory)) (error 'prompt "interaction profile is unavailable" recipe))
      (unless (and (list? profile) (for-all (lambda (p)
                                              (and (pair? p)
                                                (case (car p)
                                                  [(history) (and (list? (cdr p)) (for-all string? (cdr p)))]
                                                  [(alternate) (or (completion:source? (cdr p)) (procedure? (cdr p)))]
                                                  [(normalize validate ghost transform edge inspect) (procedure? (cdr p))]
                                                  [else #f]))) profile)
                (let unique ([xs profile]) (or (null? xs) (and (not (assq (caar xs) (cdr xs))) (unique (cdr xs))))))
        (error 'prompt "invalid interaction profile" profile))
      (values factory profile)))
  (define (queue! thunk)
    (when (null? pending) (head:run-on-main! drain!))
    (set! pending (cons thunk pending)))
  (define (resume!)
    (suspension:drain! (lambda (ex) (log:add! 'prompt:resume! (kernel:condition-text ex)))))

  (edoc "Deliver queued prompt outcomes from the ordinary pump, outside widget service and painting. Named targets run at command boundaries and may open another input request.")
  (define (drain!)
    (let ([batch (reverse pending)])
      (set! pending '())
      (for-each
        (lambda (thunk)
          (guard (ex [else (log:add! 'prompt:drain! (kernel:condition-text ex))])
            (suspension:call! head:ui-actor (lambda () (head:run-on-main! resume!)) thunk))) batch)))

  (define (context id)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (unless (and (eq? (view:kind d) 'prompt) source
                (eq? (get source 'kind) 'prompt-request)
                (equal? (get (get source 'value) 'owner) head:ui-actor))
        (error 'prompt "request is unavailable" id))
      (values source d (get source 'value))))

  (define (release! id)
    (let ([r (hashtable-ref live id #f)])
      (when r
        (runtime-closed?-set! r #t)
        (when (runtime-completion r) (completion-state:finish! (runtime-completion r) #f))
        (hashtable-delete! live id)
        ;; A host may be walking the very descriptors it is releasing.
        ;; Teardown runs after unmount/reconciliation has finished.
        (queue! (lambda () (prompt-request:close! head:ui-actor (runtime-request r)))))))

  (define (service! id frame)
    (guard (ex [else
                (let* ([d (interaction:snapshot id)] [request (and d (view:source d))]
                       [source (and request (model:snapshot request))])
                  (when source
                    (log:add! 'prompt:service! (kernel:condition-text ex))
                    (unless (hashtable-ref live id #f)
                      (queue! (lambda () (prompt-request:close! head:ui-actor request)))))
                  (release! id))])
      (let-values ([(source d value) (context id)])
        (let* ([request (get source 'id)] [generation (view:generation d)]
               [status (get value 'status)]
               [r (and (equal? id (get value 'controller))
                    (or (hashtable-ref live id #f)
                      (let*-values ([(profile-factory profile) (make-profile d (get value 'origin))]
                                    [(recipe) (get value 'provider)] [(factory) (and recipe (completion:provider recipe))]
                                    [(source) (and factory (factory (caddr recipe) (get value 'origin)))]
                                    [(r) (make-runtime request generation factory source
                                           (and source (completion-state:create source (option profile 'transform #f)))
                                           profile-factory profile #f (not (eq? status 'editing)) #f '(ghost . "") -1 "" #f "symbol" '())])
                        (when (and recipe (not factory)) (error 'prompt "completion provider is unavailable" recipe))
                        (hashtable-set! live id r) r)))])
          (when r
            ;; Re-arranging a still-mounted composition renews its lease. An
            ;; editing controller follows that lease; queued outcomes retain
            ;; the generation captured when they became terminal.
            (when (eq? status 'editing) (runtime-generation-set! r generation))
            (unless (and (eq? (runtime-factory r) (and (get value 'provider) (completion:provider (get value 'provider))))
                      (eq? (runtime-profile-factory r) (profile-factory d)))
              (release! id))
            (when (and (eq? status 'editing) (not (runtime-closed? r)))
              (refresh-completion! id r)))
          ;; The base identifies the controller, independent of mount order.
          ;; Mounting a terminal outcome never replays it, including reload.
          (unless (or (not r) (runtime-delivered? r) (runtime-closed? r) (eq? status 'editing))
            (runtime-generation-set! r generation)
            (runtime-delivered?-set! r #t)
            (when (runtime-completion r) (completion-state:finish! (runtime-completion r) (eq? status 'accepted)))
            (let ([outcome (if (eq? status 'accepted) (get value 'outcome) (get value 'origin))])
              (queue!
                (lambda ()
                  (let ([d (interaction:snapshot id)])
                    (when (and (not (runtime-closed? r)) (eq? r (hashtable-ref live id #f)) d
                            (= (runtime-generation r) (view:generation d))
                            (equal? (runtime-request r) (view:source d))
                            (assq status (widget:commands id)))
                      (widget:invoke! id status outcome)))))))))))

  (edoc "Create an unmounted prompt composition for an existing request. Options are label, multiline?, help, an interaction profile recipe and input editing-policy/mode; commands bind accepted and cancelled to explicit host targets. The request owns its views; a borrowed draft retains its lifetime."
        (request model "base prompt request") (options list "portable presentation options")
        (commands list "explicit named outcome targets") (returns model "prompt view"))
  (define (create! request options commands)
    (unless (and (list? options)
              (for-all (lambda (p) (and (pair? p)
                                     (case (car p) [(label help choices) (string? (cdr p))]
                                       [(multiline?) (boolean? (cdr p))]
                                       [(mode) (string? (cdr p))]
                                       [(profile) (and (list? (cdr p)) (= (length (cdr p)) 3) (symbol? (cadr p))
                                                    (integer? (caddr p)) (exact? (caddr p)) (> (caddr p) 0))]
                                       [(editing-policy) (and (list? (cdr p)) (= (length (cdr p)) 2) (symbol? (cadr p))
                                                           (integer? (caddr p)) (exact? (caddr p)) (> (caddr p) 0))]
                                       [else #f]))) options)
              (let unique ([xs options]) (or (null? xs) (and (not (assq (caar xs) (cdr xs))) (unique (cdr xs))))))
      (error 'create! "invalid prompt options" options))
    (let* ([packet (model:snapshots (list request))] [r (caddr (caadr packet))]
           [value (and r (get r 'value))])
      (unless (and r (eq? (get r 'kind) 'prompt-request) (eq? (get value 'status) 'editing) (not (get value 'controller))
                (equal? (get value 'owner) head:ui-actor)) (error 'create! "request is unavailable" request))
      (let ([created '()])
        (define (create-view! who source kind schema options state scope)
          (let ([id (view:create! who source kind schema options state scope)])
            (set! created (cons id created)) id))
        (guard (ex [else
                    (for-each
                      (lambda (id)
                        (guard (ignored [else (void)])
                          (let ([r (caddr (caadr (model:snapshots (list id))))])
                            (when r (view:retire! head:ui-actor id (get r 'revision)))))) created)
                    (raise ex)])
          (let* ([who head:ui-actor] [source (get value 'draft)]
                 [multiline? (cond [(assq 'multiline? options) => cdr] [else #f])]
                 [root-options (append (list (cons 'commands commands))
                                 (filter (lambda (p) (memq (car p) '(profile choices))) options)
                                 (if (assq 'choices options) '((capture . full)) '()))]
                 [root (create-view! who request 'prompt 1 root-options '() request)]
                 [input (create-view! who #f (if (assq 'choices options) 'column 'row) 1 '((spacing . normal)) '() request)]
                 [label (create-view! who #f 'label 1
                          (list (cons 'text (cond [(assq 'label options) => cdr] [else "Input:"]))
                            (cons 'wrap (and (assq 'choices options) #t))) '() request)]
                 [entry (create-view! who source (if multiline? 'editor 'entry) 1
                          (filter values (list (assq 'mode options)
                                           (let ([p (assq 'editing-policy options)]) (and p (cons 'policy (cdr p))))))
                          (if multiline? '((0 . 0) (0 . 0) (0 . 0) #f) '((0 . 0) (0 . 0))) request)]
                 [choices (create-view! who request 'prompt-completion 1
                            (list (cons 'commands (list (list 'select root 'choose '()) (list 'cancel root 'cancel '())))) '() request)]
                 [help (create-view! who request 'prompt-help 1
                         (list (cons 'text (cond [(assq 'help options) => cdr] [else ""]))) '() request)])
            (let-values ([(status rows)
                          (view:arrange! who
                            (list (list input 0 (list (list 'label label 'fit) (list 'entry entry '(grow 1))) '((spacing . normal)))
                              (list root 0 (list (list 'choices choices 'fit) (list 'help help 'fit) (list 'input input '(grow 1))) root-options)) '())])
              (unless (eq? status 'applied) (error 'create! "cannot compose prompt" status)))
            (let ([status (prompt-request:bind! who request (get r 'revision) root)])
              (unless (eq? status 'applied) (error 'create! "request controller changed" status))) root)))))

  (define (draft-context id)
    (let* ([entry (widget:descendant id 'input 'entry)] [d (interaction:snapshot entry)])
      (let-values ([(source d) (text-control:context entry (view:kind d) 'current)])
        (let* ([mirror (text-control:mirror source)] [lines (text-control:lines source)]
               [points (text-source:rebase (list (car (view:state d)))
                         (text-source:changes mirror (or (view:basis d) (text-control:revision source)) (text-control:revision source)))]
               [p (and points (car points))])
          (unless p (error 'prompt "draft caret history is unavailable"))
          (values entry source d (string:join (vector->list lines) "\n")
            (+ (cdr p) (fold-left + 0 (map (lambda (r) (+ 1 (string-length (vector-ref lines r)))) (iota (car p))))))))))
  (define (refresh-completion! id r)
    (let-values ([(entry source d text position) (draft-context id)])
      (let ([signature (list (text-control:revision source) position
                         (and (completion:source? (runtime-source r))
                           ((completion:source-basis (runtime-source r)))))])
        (unless (or (text-control:pending? entry) (equal? signature (runtime-signature r)))
          (when (and (>= (runtime-history-index r) 0)
                  (not (string=? text (list-ref (option (runtime-profile r) 'history '()) (runtime-history-index r)))))
            (runtime-history-index-set! r -1))
          (let ([hint (cond [(option (runtime-profile r) 'ghost #f) => (lambda (ghost) (or (ghost text position) ""))] [else ""])])
            (unless (string? hint) (error 'prompt "hint provider must return text or false"))
            (runtime-message-set! r (cons 'ghost hint)))
          (when (runtime-completion r) (completion-state:refresh! (runtime-completion r) text position) (refresh-kind! r))
          (runtime-signature-set! r signature)
          (repaint-completion! id)))))
  (define (repaint-completion! id)
    (define (repaint-tree! child)
      (widget:repaint! child #t)
      (let ([d (interaction:snapshot child)]) (when d (for-each (lambda (c) (repaint-tree! (cadr c))) (view:children d)))))
    (for-each (lambda (name) (repaint-tree! (widget:descendant id name))) '(choices help)))
  (define (refresh-kind! r)
    (let* ([snapshot (completion-state:snapshot (runtime-completion r))]
           [source (or (list-ref snapshot 6) (runtime-source r))] [kind (and (completion:source? source) (completion:source-kind source))]
           [context (and (completion:source? source) (completion:source-context source))])
      (runtime-kind-set! r (or (if (procedure? kind) (kind (cadr snapshot) (caddr snapshot)) kind) "symbol"))
      (runtime-context-set! r (if context (context (cadr snapshot) (caddr snapshot)) '()))))
  (define (input-position text offset)
    (let loop ([at 0] [row 0] [column 0])
      (if (= at offset) (cons row column)
        (if (char=? (string-ref text at) #\newline) (loop (+ at 1) (+ row 1) 0) (loop (+ at 1) row (+ column 1))))))
  (define (apply-completion! id r entry source d)
    (let ([snapshot (completion-state:snapshot (runtime-completion r))])
      (apply-text! id r entry source d (cadr snapshot) (caddr snapshot))))
  (define (apply-text! id r entry source d text caret)
    (guard (ex [else
                (runtime-signature-set! r #f)
                (guard (ignored [else (void)]) (refresh-completion! id r))
                (raise ex)])
      (let* ([position (input-position text caret)] [old (text-control:lines source)]
             [lines (list->vector (string:lines text))])
        (if (not (equal? old lines))
          (let-values ([(span replacement) (text:difference old lines)])
            (text-control:submit! entry source d old (text-control:revision source) span replacement
              (list #f "Prompt input" (cons 'revision (text-control:revision source))) (list position)
              (lambda (ps) (if (eq? (view:kind d) 'entry) (list (car ps) (car ps)) (list (car ps) (car ps) (car ps) #f)))))
          (if (eq? (view:kind d) 'entry) (entry:select! entry (cdr position) (cdr position)) (editor:select! entry position position)))
        (refresh-completion! id r) (repaint-completion! id))))

  (define (editing-runtime id)
    (let-values ([(source d value) (context id)])
      (let ([r (hashtable-ref live id #f)])
        (and r (eq? (get value 'status) 'editing)
          (not (runtime-delivered? r)) (not (runtime-closed? r))
          (if (and (eq? (runtime-factory r) (and (get value 'provider) (completion:provider (get value 'provider))))
                (eq? (runtime-profile-factory r) (profile-factory d))) r
            (begin (release! id) #f))))))

  (edoc "Normalize this prompt's current token, then cycle equivalent spellings or completion pages. Work runs outside painting against a captured draft revision."
        (id model "prompt controller") (backwards? boolean "visit the preceding live-search hit"))
  (define (complete! id backwards?)
    (let ([r (editing-runtime id)])
      (when (and r (runtime-completion r))
        (refresh-completion! id r)
        (let-values ([(entry source d text position) (draft-context id)])
          (completion-state:normalize! (runtime-completion r)
            (if backwards? (option (runtime-profile r) 'alternate (runtime-source r)) (runtime-source r)) backwards?)
          (refresh-kind! r)
          (apply-completion! id r entry source d)))))

  (edoc "Choose a completion from the displayed generation, refusing after another draft edit or provider result."
        (id model "prompt controller") (generation integer "displayed completion generation")
        (value string "insertion text") (returns boolean))
  (define (choose! id generation value)
    (let ([r (editing-runtime id)])
      (and r (runtime-completion r)
        (begin
          (refresh-completion! id r)
          (let-values ([(entry source d text position) (draft-context id)])
            (and (completion-state:choose! (runtime-completion r) generation value)
              (begin (apply-completion! id r entry source d) #t)))))))

  (edoc "Browse this request's captured history at an input edge; within multiline input, move the editor caret vertically. Returning past the newest item restores the unfinished draft."
        (id model "prompt controller") (direction (one-of previous next) "history direction"))
  (define (history! id direction)
    (unless (memq direction '(previous next)) (error 'history! "invalid direction"))
    (let ([r (editing-runtime id)])
      (when r
        (refresh-completion! id r)
        (let-values ([(entry source d text caret) (draft-context id)])
          (let* ([row (car (input-position text caret))] [single? (eq? (view:kind d) 'entry)]
                 [history (option (runtime-profile r) 'history '())] [index (runtime-history-index r)]
                 [next (+ index (if (eq? direction 'previous) 1 -1))])
            (if (and (not single?) (= index -1)
                  (if (eq? direction 'previous) (> row 0) (< row (- (vector-length (text-control:lines source)) 1))))
              (editor:move! entry (if (eq? direction 'previous) 'up 'down))
              (when (and (<= -1 next) (< next (length history)) (not (and (= index -1) (eq? direction 'next))))
                (when (= index -1) (runtime-stash-set! r text))
                (let ([text (if (= next -1) (runtime-stash r) (list-ref history next))])
                  (apply-text! id r entry source d text (string-length text))
                  (runtime-history-index-set! r next)))))))))

  (edoc "Move to a prompt's line edge. A registered profile may define repeated-edge behavior, such as Scheme input's indentation and whole-expression endpoints."
        (id model "prompt controller") (direction (one-of beginning end) "edge"))
  (define (edge! id direction)
    (unless (memq direction '(beginning end)) (error 'edge! "invalid edge"))
    (let ([r (editing-runtime id)])
      (when r
        (let-values ([(entry source d text caret) (draft-context id)])
          (let* ([move (option (runtime-profile r) 'edge #f)] [turn (car (keymap:command-state))]
                 [last (runtime-edge r)])
            (if move
              (apply-text! id r entry source d text (move direction text caret (and last (eq? direction (cdr last)) (= turn (+ 1 (car last))))))
              ((if (eq? (view:kind d) 'entry) entry:move! editor:move!) entry (if (eq? direction 'beginning) 'home 'end)))
            (runtime-edge-set! r (cons turn direction)))))))

  (edoc "Inspect the name at this prompt's current caret through its explicit interaction profile."
        (id model "prompt controller"))
  (define (inspect! id)
    (let* ([r (editing-runtime id)] [inspect (and r (option (runtime-profile r) 'inspect #f))])
      (when inspect
        (let-values ([(entry source d text caret) (draft-context id)]) (inspect text caret)))))

  (edoc "Insert a newline through this prompt's multiline editor and its normal editing policy. Single-line prompts leave their input unchanged."
        (id model "prompt controller"))
  (define (newline! id)
    (when (editing-runtime id)
      (let ([entry (widget:descendant id 'input 'entry)])
        (when (eq? (view:kind (interaction:snapshot entry)) 'editor) (editor:insert! entry "\n")))))

  ;; The head owns its API corpus and prepared labels. Only authored input and
  ;; the portable provider recipe belong in the base; painting never ships rows.
  (define-record-type projection (fields id controller snapshot message kind layout (mutable width) (mutable rows)))
  (define (completion-data id source inputs)
    (let* ([controller (get (get source 'value) 'controller)] [r (hashtable-ref live controller #f)])
      (make-projection id controller (and r (runtime-completion r) (completion-state:snapshot (runtime-completion r)))
        (if r (runtime-message r) '(ghost . "")) (if r (runtime-kind r) "symbol")
        (option (view:options (interaction:snapshot id)) 'layout 'columns) #f '#())))
  (define completion-table (table:make '#("Completion" "Details") '#(10 7) 0 '(1) '#(text text)))
  (define completion-column (table:make '#("Completion") '#(10) 0 '() '#(text)))
  (define (table-rows candidates width)
    (define (cells candidate)
      (if (completion:candidate? candidate)
        (or (completion:candidate-cells candidate) (vector (completion:candidate-label candidate) "")) (vector candidate "")))
    (define (cell candidate column) (vector-ref (cells candidate) column))
    (if (null? candidates) '#()
      (let-values ([(format-row columns) (table:layout
                                           (if (for-all (lambda (c) (string=? (cell c 1) "")) candidates) completion-column completion-table)
                                           '() candidates cell width)])
        (list->vector
          (cons (let ([text (format-row #f)]) (completion-layout:make-row text (make-vector (string-length text) 'header) #f '()))
            (map (lambda (candidate)
                   (let* ([text (format-row candidate)] [faces (make-vector (string-length text) 'plain)]
                          [styles (and (completion:candidate? candidate) (completion:candidate-styles candidate))]
                          [start (if (completion:candidate? candidate) (completion:candidate-value candidate) candidate)])
                     (let columns-loop ([columns columns] [offset 0])
                       (when (pair? columns)
                         (let* ([column (caar columns)] [source (cell candidate column)] [size (- (caddar columns) (cadar columns))]
                                [fitted (glyph:fit source size)] [source-offset (if (zero? column) 0 (+ 2 (string-length (cell candidate 0))))])
                           (do ([i 0 (+ i 1)]) ((= i (string-length fitted)))
                             (when (< (+ offset i) (vector-length faces))
                               (vector-set! faces (+ offset i)
                                 (if (and styles (< i (string-length source)) (< (+ source-offset i) (vector-length styles)))
                                   (vector-ref styles (+ source-offset i)) (if (zero? column) 'plain 'chrome)))))
                           (columns-loop (cdr columns) (+ offset (string-length fitted) 2)))))
                     (completion-layout:make-row text faces #f (list (list 0 (string-length text) start))))) candidates))))))
  (define (rows! data width)
    (unless (equal? width (projection-width data))
      (projection-width-set! data width)
      (let ([candidates (or (and (projection-snapshot data) (list-ref (projection-snapshot data) 3)) '())])
        (projection-rows-set! data (if (eq? (projection-layout data) 'table) (table-rows candidates (max 1 width))
                                     (completion-layout:format-columns candidates (max 1 width) values (lambda (s) #f))))))
    (projection-rows data))
  (define (choice-page data d width height range)
    (let* ([rows (rows! data width)] [count (vector-length rows)]
           [pages (max 1 (div (+ count (max 1 height) -1) (max 1 height)))]
           [snapshot (projection-snapshot data)] [page (mod (if snapshot (list-ref snapshot 5) 0) pages)]
           [start (min count (+ (* page (max 1 height)) (car range)))]
           [rows (map (lambda (n) (vector-ref rows (+ start n))) (iota (min (cdr range) (- count start))))])
      (list (projection-controller data) (and snapshot (car snapshot)) rows (projection-id data))))
  (define (choice-render data d width height range) (map completion-layout:row-text (caddr data)))
  (define (choice-decorate data d width height range)
    (append (apply append
              (map (lambda (row index)
                     (let ([text (completion-layout:row-text row)] [styles (completion-layout:row-styles row)])
                       (if (not styles) '()
                         (let loop ([clusters (glyph:clusters text)] [character 0] [column 0] [out '()])
                           (if (null? clusters) (reverse out)
                             (loop (cdr clusters) (+ character (caar clusters)) (+ column (cdar clusters))
                               (cons (list (list column (+ index (car range)) (cdar clusters) 1) (vector-ref styles character)) out)))))))
                (caddr data) (iota (length (caddr data)))))
      (let ([hover (hashtable-ref hovered (cadddr data) #f)])
        (if (and hover (equal? (car hover) (cadr data))) (list (list (cdr hover) 'hover)) '()))))
  (define (choice-at frame x y)
    (let* ([data (widget:frame-data frame)] [rows (caddr data)]
           [row (- y (- (cadr (widget:frame-clip frame)) (cadr (widget:frame-rect frame))))])
      (and (<= 0 row) (< row (length rows))
        (let* ([r (list-ref rows row)] [text (completion-layout:row-text r)]
               [choice (find (lambda (c) (<= (glyph:cells (substring text 0 (car c))) x
                                           (- (glyph:cells (substring text 0 (cadr c))) 1))) (completion-layout:row-choices r))])
          (and choice (list choice text))))))
  (define (choice-bindings frame x y)
    (let ([hit (choice-at frame x y)] [data (widget:frame-data frame)])
      (if hit (list (list '(click primary ()) (keymap:call widget:invoke! (cadddr data) 'select (cadr data) (caddr (car hit))))) '())))
  (define (choice-event! id source d event)
    (and (eq? (car event) 'pointer)
      (let* ([frame (widget:event-frame)] [x (list-ref event 4)] [y (list-ref event 5)]
             [hit (and (not (eq? (cadr event) 'leave)) (choice-at frame x y))]
             [hover (and hit (let* ([choice (car hit)] [text (cadr hit)]
                                    [a (glyph:cells (substring text 0 (car choice)))]
                                    [b (glyph:cells (substring text 0 (cadr choice)))])
                               (list (cadr (widget:frame-data frame)) a y (- b a) 1)))])
        (unless (equal? hover (hashtable-ref hovered id #f))
          (if hover (hashtable-set! hovered id hover) (hashtable-delete! hovered id)) (widget:repaint! id))
        (and hit (eq? (cadr event) 'press) (eq? (caddr event) 'primary)
          (begin (keymap:run! (cadar (choice-bindings frame x y))) #t)))))
  (define (help-text data d)
    (let ([note (and (projection-snapshot data) (list-ref (projection-snapshot data) 4))]
          [message (projection-message data)])
      (cond [(eq? (car message) 'validation) (cdr message)]
        [(and note (not (string=? note ""))) note]
        [(and (projection-snapshot data) (list-ref (projection-snapshot data) 3))
         => (lambda (candidates) (format "~a matches of ~a" (length candidates) (projection-kind data)))]
        [(not (string=? (cdr message) "")) (cdr message)]
        [else (get (view:options d) 'text)])))

  (edoc "Register an outer host for linear prompt callers. Preparing captures (values parent-request origin attach); attach receives a request and root view and returns its cleanup thunk. The host owns placement and focus, never the prompt's input loop."
        (prepare procedure "capture origin and an attachment capability"))
  (define (register-host! prepare)
    (unless (procedure? prepare) (error 'register-host! "expected a host preparation procedure"))
    (kernel:registry-add! hosts prepare))

  (define (accepted! id outcome)
    (let ([entry (hashtable-ref tickets id #f)])
      (when entry (suspension:resolve! (cdr entry) (string:join (vector->list (cadr outcome)) "\n")))))
  (define (cancelled! id origin) (abandon! id))
  (define (abandon! id)
    (let ([entry (hashtable-ref tickets id #f)])
      (when entry (suspension:cancel! (cdr entry)))))

  (edoc "Read authored text through the installed host on the ordinary pump. This linear adapter parks its command, returning text or false after cancellation. Embedded applications bind named targets directly instead."
        (label string "input label") (initial string "initial authored text")
        (provider datum "completion recipe or false") (options list "prompt control options except label")
        (returns (or string #f)) (prompts))
  (define (read! label initial provider options)
    (let ([prepare (kernel:registry-find hosts (lambda (entry) #t))])
      (unless prepare (error 'read! "no prompt host is installed"))
      (let-values ([(parent origin attach) (prepare)])
        (suspension:wait!
          (lambda (ticket)
            (let ([request #f] [receiver #f] [detach #f])
              (define (cleanup!)
                (when receiver (hashtable-delete! tickets receiver))
                (dynamic-wind void
                  (lambda () (when detach (let ([close detach]) (set! detach #f) (close))))
                  (lambda () (when request (prompt-request:close! head:ui-actor request)))))
              (guard (ex [else (cleanup!) (raise ex)])
                (set! request (prompt-request:create! head:ui-actor parent #f initial origin provider))
                (set! receiver (view:create! head:ui-actor request 'prompt-continuation 1 '((modal . #t)) '() request))
                (hashtable-set! tickets receiver (cons prepare ticket))
                (let ([control (create! request (cons (cons 'label label) options)
                                 (list (list 'accepted receiver 'accepted '()) (list 'cancelled receiver 'cancelled '())))])
                  (let-values ([(status rows)
                                (view:arrange! head:ui-actor
                                  (list (list receiver 0 (list (list 'prompt control '(grow 1))) '((modal . #t)))) '())])
                    (unless (eq? status 'applied) (error 'read! "cannot compose prompt receiver" status))))
                (let ([close (attach request receiver)])
                  (unless (procedure? close) (error 'read! "host must return a cleanup thunk"))
                  (set! detach close))
                (let* ([control (widget:descendant receiver 'prompt)]
                       [entry (widget:descendant control 'input 'entry)]
                       [position (input-position initial (string-length initial))])
                  (if (eq? (view:kind (interaction:snapshot entry)) 'entry)
                    (entry:select! entry (cdr position) (cdr position))
                    (editor:select! entry position position)))
                cleanup!)))))))

  (edoc "Accept this prompt's exact authored draft revision once. A changed or closed request refuses; its named accepted target runs later on the ordinary pump with (revision lines origin)."
        (id model "prompt view") (returns symbol))
  (define (accept! id)
    (service! id #f)
    (let ([r (editing-runtime id)])
      (when r
        (let-values ([(entry source d text caret) (draft-context id)])
          (let* ([normalize (option (runtime-profile r) 'normalize values)] [value (normalize text)]
                 [validate (option (runtime-profile r) 'validate #f)])
            (unless (string? value) (error 'accept! "normalizer must return text"))
            (unless (string=? value text) (apply-text! id r entry source d value (string-length value)))
            (let ([problem (and validate (validate value))])
              (if problem
                (begin
                  (unless (string? problem) (error 'accept! "validator must return false or text"))
                  (runtime-message-set! r (cons 'validation (string-append "[" problem "]"))) (repaint-completion! id) 'invalid)
                (accept-draft! id))))))))
  (define (accept-draft! id)
    (let-values ([(source d value) (context id)])
      (let ([draft (text-source:lookup (cadr (get value 'draft)))])
        (unless draft (error 'accept! "draft is unavailable"))
        (let ([status (if (and (option (view:options d) 'choices #f)
                            (not (valid-choice? (option (view:options d) 'choices #f) (string:join (vector->list (text-source:lines draft)) "\n")))) 'invalid
                        (prompt-request:accept! head:ui-actor (get source 'id) (get source 'revision) (text-source:revision draft)))])
          ;; Resolve the terminal outcome before the next input can acquire a
          ;; route. An asynchronous invalidation alone can leave a fast chord
          ;; addressed to the just-accepted prompt.
          (model:snapshots (list (get source 'id)))
          (service! id #f) (head:wake-main!) status))))

  (edoc "Cancel this prompt and its nested requests. The named cancelled target runs later with the captured origin; repeated cancellation has no effect."
        (id model "prompt view") (returns boolean))
  (define (cancel! id)
    (let-values ([(source d value) (context id)])
      (let ([changed? (prompt-request:cancel! head:ui-actor (get source 'id))])
        (model:snapshots (list (get source 'id)))
        (service! id #f) (head:wake-main!) changed?)))

  (define (valid-choice? choices text)
    (and (string? text) (= (string-length text) 1)
      (exists (lambda (c) (char-ci=? c (string-ref text 0))) (string->list choices))))
  (define (capture-event! id source d event)
    (let ([choices (option (view:options d) 'choices #f)])
      (and choices (memq (car event) '(key text))
        (let ([text (if (eq? (car event) 'text) (cadr event) (and (= (length event) 3) (caddr event)))])
          (when (valid-choice? choices text)
            (entry:set-text! (widget:descendant id 'input 'entry) text) (accept! id)) #t))))

  (edoc "Ask for one of the stated character choices on the ordinary pump. Escape or C-g cancels with false; arrows and unrelated text do not answer the question."
        (question string "question and choice labels") (allowed string "case-insensitive characters")
        (returns (or char #f)) (prompts))
  (define (key! question allowed)
    (unless (and (string? allowed) (> (string-length allowed) 0)) (error 'key! "expected allowed characters"))
    (and (head:input-live?)
      (let ([answer (read! question "" #f (list (cons 'choices allowed)))])
        (and answer (> (string-length answer) 0) (string-ref answer 0)))))

  (edoc "Install prompt composition, request lifetime service and inspectable accept/cancel bindings." (public))
  (define (init!)
    (for-each (lambda (type) (register-presentation! type table-presentation)) '(file buffer buffer-name (one-of partial full)))
    (widget:register! 'prompt-completion 1
      (append (layout:container 'y)
        (list (cons 'service presentation-service!)
          (cons 'release (lambda (id) (hashtable-delete! presentation-factories id))))))
    (widget:register! 'prompt-continuation 1
      (append (layout:container 'y)
        (list (cons 'contexts (lambda (id d) (option (view:options d) 'contexts '())))
          (cons 'release abandon!) (cons 'actions (list (cons 'accepted accepted!) (cons 'cancelled cancelled!))))))
    (widget:register! 'prompt-choices 1
      (list (cons 'prepare completion-data) (cons 'viewport choice-page) (cons 'render choice-render)
        (cons 'decorate choice-decorate) (cons 'event choice-event!) (cons 'pointer-bindings choice-bindings)
        (cons 'release (lambda (id) (hashtable-delete! hovered id)))
        (cons 'measure (lambda (data d axis cross child)
                         (if (eq? axis 'y) (list 0 (min 8 (vector-length (rows! data cross)))) '(0 1))))))
    (widget:register! 'prompt-help 1
      (list (cons 'prepare completion-data)
        (cons 'measure (lambda (data d axis cross child)
                         (if (eq? axis 'y) (if (string=? (help-text data d) "") '(0 0) '(1 1)) '(0 1))))
        (cons 'render (lambda (data d width height range) (if (zero? (car range)) (list (glyph:fit (help-text data d) width)) '())))
        (cons 'decorate (lambda (data d width height range) (list (list (list 0 0 width height) 'ghost))))))
    (widget:register! 'prompt 1
      (append (layout:container 'y)
        (list (cons 'capture-contexts '(widget-prompt)) (cons 'capture-event capture-event!) (cons 'service service!) (cons 'release release!)
          (cons 'actions (list (cons 'accept accept!) (cons 'cancel cancel!) (cons 'choose choose!))))))
    (keymap:bind-default! 'widget-prompt "RET" (keymap:call accept! widget:target))
    (keymap:bind-default! 'widget-prompt "TAB" (keymap:call complete! widget:target #f))
    (keymap:bind-default! 'widget-prompt "S-TAB" (keymap:call complete! widget:target #t))
    (keymap:bind-default! 'widget-prompt "C-g" (keymap:call cancel! widget:target))
    (keymap:bind-default! 'widget-prompt "ESC" (keymap:call cancel! widget:target))
    (keymap:bind-default! 'widget-prompt "UP" (keymap:call history! widget:target 'previous))
    (keymap:bind-default! 'widget-prompt "DOWN" (keymap:call history! widget:target 'next))
    (keymap:bind-default! 'widget-prompt "C-a" (keymap:call edge! widget:target 'beginning))
    (keymap:bind-default! 'widget-prompt "HOME" (keymap:call edge! widget:target 'beginning))
    (keymap:bind-default! 'widget-prompt "C-e" (keymap:call edge! widget:target 'end))
    (keymap:bind-default! 'widget-prompt "END" (keymap:call edge! widget:target 'end))
    (keymap:bind-default! 'widget-prompt "M-." (keymap:call inspect! widget:target))
    (keymap:bind-default! 'widget-prompt "M-RET" (keymap:call newline! widget:target))))
