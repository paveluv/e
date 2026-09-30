;; Prompt controls own no keyboard reader or window. The host places the tree.
(import (only (foundation edoc) elibrary))
(elibrary (head prompt-control)
  (export accept! cancel! choose! complete! create! drain! init!)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (foundation string) string:) (prefix (foundation text) text:)
          (prefix (head completion) completion:) (prefix (head completion-layout) completion-layout:)
          (prefix (head completion-state) completion-state:) (prefix (head editor) editor:)
          (prefix (head entry) entry:) (prefix (head head) head:) (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:) (prefix (head layout) layout:)
          (prefix (head suspension) suspension:) (prefix (head text-control) text-control:) (prefix (head text-source) text-source:)
          (prefix (head widget) widget:) (prefix (service log) log:)
          (prefix (service prompt-request) prompt-request:) (prefix (state model) model:)
          (prefix (state view) view:) (prefix (sys glyph) glyph:))

  (define (get r key) (cdr (assq key r)))
  (define live (make-hashtable equal-hash equal?))
  (define hovered (make-hashtable equal-hash equal?))
  (define pending '())
  (define-record-type runtime (fields request generation factory source completion
                                (mutable signature) (mutable delivered?) (mutable closed?)))
  (define (queue! thunk)
    (when (null? pending) (head:run-on-main! drain!))
    (set! pending (cons thunk pending)))
  (define (resume!)
    (suspension:drain! (lambda (ex) (log:add! 'prompt-control:resume! (kernel:condition-text ex)))))

  (edoc "Deliver queued prompt outcomes from the ordinary pump, outside widget service and painting. Named targets run at command boundaries and may open another input request.")
  (define (drain!)
    (let ([batch (reverse pending)])
      (set! pending '())
      (for-each
        (lambda (thunk)
          (guard (ex [else (log:add! 'prompt-control:drain! (kernel:condition-text ex))])
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
                    (log:add! 'prompt-control:service! (kernel:condition-text ex))
                    (unless (hashtable-ref live id #f)
                      (queue! (lambda () (prompt-request:close! head:ui-actor request)))))
                  (release! id))])
      (let-values ([(source d value) (context id)])
        (let* ([request (get source 'id)] [generation (view:generation d)]
               [status (get value 'status)]
               [r (and (equal? id (get value 'controller))
                    (or (hashtable-ref live id #f)
                      (let* ([recipe (get value 'provider)] [factory (and recipe (completion:provider recipe))]
                             [source (and factory (factory (caddr recipe) (get value 'origin)))]
                             [r (make-runtime request generation factory source
                                  (and source (completion-state:create source #f)) #f (not (eq? status 'editing)) #f)])
                        (when (and recipe (not factory)) (error 'prompt "completion provider is unavailable" recipe))
                        (hashtable-set! live id r) r)))])
          (when r
            (unless (eq? (runtime-factory r) (and (get value 'provider) (completion:provider (get value 'provider))))
              (release! id))
            (when (and (runtime-completion r) (eq? status 'editing) (not (runtime-closed? r)))
              (refresh-completion! id r)))
          ;; The base identifies the controller, independent of mount order.
          ;; Mounting a terminal outcome never replays it, including reload.
          (unless (or (not r) (runtime-delivered? r) (runtime-closed? r) (eq? status 'editing))
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

  (edoc "Create an unmounted prompt composition for an existing request. Options are label, multiline? and help; commands bind accepted and cancelled to explicit host targets. The request owns all control views, while a borrowed draft retains its own lifetime."
        (request model "base prompt request") (options list "portable presentation options")
        (commands list "explicit named outcome targets") (returns model "prompt view"))
  (define (create! request options commands)
    (unless (and (list? options)
              (for-all (lambda (p) (and (pair? p)
                                     (case (car p) [(label help) (string? (cdr p))]
                                       [(multiline?) (boolean? (cdr p))] [else #f]))) options)
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
                 [root (create-view! who request 'prompt 1 (list (cons 'commands commands)) '() request)]
                 [input (create-view! who #f 'row 1 '((spacing . normal)) '() request)]
                 [label (create-view! who #f 'label 1
                          (list (cons 'text (cond [(assq 'label options) => cdr] [else "Input:"]))) '() request)]
                 [entry (create-view! who source (if multiline? 'editor 'entry) 1 '()
                          (if multiline? '((0 . 0) (0 . 0) (0 . 0) #f) '((0 . 0) (0 . 0))) request)]
                 [choices (create-view! who request 'prompt-choices 1 '() '() request)]
                 [help (create-view! who request 'prompt-help 1
                         (list (cons 'text (cond [(assq 'help options) => cdr] [else ""]))) '() request)])
            (let-values ([(status rows)
                          (view:arrange! who
                            (list (list input 0 (list (list 'label label 'fit) (list 'entry entry '(grow 1))) '((spacing . normal)))
                              (list root 0 (list (list 'choices choices 'fit) (list 'help help 'fit) (list 'input input '(grow 1))) (list (cons 'commands commands)))) '())])
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
      (let ([signature (list (text-control:revision source) position)])
        (unless (or (text-control:pending? entry) (equal? signature (runtime-signature r)))
          (completion-state:refresh! (runtime-completion r) text position)
          (runtime-signature-set! r signature)
          (repaint-completion! id)))))
  (define (repaint-completion! id)
    (for-each (lambda (name) (widget:repaint! (widget:descendant id name) #t)) '(choices help)))
  (define (input-position text offset)
    (let loop ([at 0] [row 0] [column 0])
      (if (= at offset) (cons row column)
        (if (char=? (string-ref text at) #\newline) (loop (+ at 1) (+ row 1) 0) (loop (+ at 1) row (+ column 1))))))
  (define (apply-completion! id r entry source d)
    (guard (ex [else
                (runtime-signature-set! r #f)
                (guard (ignored [else (void)]) (refresh-completion! id r))
                (raise ex)])
      (let* ([snapshot (completion-state:snapshot (runtime-completion r))] [text (cadr snapshot)]
             [position (input-position text (caddr snapshot))] [old (text-control:lines source)]
             [lines (list->vector (string:lines text))])
        (if (not (equal? old lines))
          (let-values ([(span replacement) (text:difference old lines)])
            (text-control:submit! entry source d old (text-control:revision source) span replacement
              (list #f "Complete input" (cons 'revision (text-control:revision source))) (list position)
              (lambda (ps) (if (eq? (view:kind d) 'entry) (list (car ps) (car ps)) (list (car ps) (car ps) (car ps) #f)))))
          (if (eq? (view:kind d) 'entry) (entry:select! entry (cdr position) (cdr position)) (editor:select! entry position position)))
        (repaint-completion! id))))

  (define (editing-runtime id)
    (let-values ([(source d value) (context id)])
      (let ([r (hashtable-ref live id #f)])
        (and r (eq? (get value 'status) 'editing) (runtime-completion r)
          (not (runtime-delivered? r)) (not (runtime-closed? r))
          (if (eq? (runtime-factory r) (completion:provider (get value 'provider))) r
            (begin (release! id) #f))))))

  (edoc "Normalize this prompt's current token, then cycle equivalent spellings or completion pages. Work runs outside painting against a captured draft revision."
        (id model "prompt controller") (backwards? boolean "visit the preceding live-search hit"))
  (define (complete! id backwards?)
    (let ([r (editing-runtime id)])
      (when r
        (refresh-completion! id r)
        (let-values ([(entry source d text position) (draft-context id)])
          (completion-state:normalize! (runtime-completion r) (runtime-source r) backwards?)
          (apply-completion! id r entry source d)))))

  (edoc "Choose a completion from the displayed generation, refusing after another draft edit or provider result."
        (id model "prompt controller") (generation integer "displayed completion generation")
        (value string "insertion text") (returns boolean))
  (define (choose! id generation value)
    (let ([r (editing-runtime id)])
      (and r
        (begin
          (refresh-completion! id r)
          (let-values ([(entry source d text position) (draft-context id)])
            (and (completion-state:choose! (runtime-completion r) generation value)
              (begin (apply-completion! id r entry source d) #t)))))))

  ;; The head owns its API corpus and prepared labels. Only authored input and
  ;; the portable provider recipe belong in the base; painting never ships rows.
  (define-record-type projection (fields id controller snapshot (mutable width) (mutable rows)))
  (define (completion-data id source inputs)
    (let* ([controller (get (get source 'value) 'controller)] [r (hashtable-ref live controller #f)])
      (make-projection id controller (and r (runtime-completion r) (completion-state:snapshot (runtime-completion r))) #f '#())))
  (define (rows! data width)
    (unless (equal? width (projection-width data))
      (projection-width-set! data width)
      (projection-rows-set! data (completion-layout:format-columns
                                   (or (and (projection-snapshot data) (list-ref (projection-snapshot data) 3)) '())
                                   (max 1 width) values (lambda (s) #f))))
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
      (if hit (list (list '(click primary ()) (keymap:call choose! (car data) (cadr data) (caddr (car hit))))) '())))
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
    (let ([note (and (projection-snapshot data) (list-ref (projection-snapshot data) 4))])
      (if (and note (not (string=? note ""))) note (get (view:options d) 'text))))

  (edoc "Accept this prompt's exact authored draft revision once. A changed or closed request refuses; its named accepted target runs later on the ordinary pump with (revision lines origin)."
        (id model "prompt view") (returns symbol))
  (define (accept! id)
    (let-values ([(source d value) (context id)])
      (let ([draft (text-source:lookup (cadr (get value 'draft)))])
        (unless draft (error 'accept! "draft is unavailable"))
        (let ([status (prompt-request:accept! head:ui-actor (get source 'id) (get source 'revision) (text-source:revision draft))])
          (head:wake-main!) status))))

  (edoc "Cancel this prompt and its nested requests. The named cancelled target runs later with the captured origin; repeated cancellation has no effect."
        (id model "prompt view") (returns boolean))
  (define (cancel! id)
    (let-values ([(source d value) (context id)])
      (let ([changed? (prompt-request:cancel! head:ui-actor (get source 'id))])
        (head:wake-main!) changed?)))

  (edoc "Install prompt composition, request lifetime service and inspectable accept/cancel bindings.")
  (define (init!)
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
        (list (cons 'capture-contexts '(widget-prompt)) (cons 'service service!) (cons 'release release!)
          (cons 'actions (list (cons 'accept accept!) (cons 'cancel cancel!))))))
    (keymap:bind-default! 'widget-prompt "RET" (keymap:call accept! widget:target))
    (keymap:bind-default! 'widget-prompt "TAB" (keymap:call complete! widget:target #f))
    (keymap:bind-default! 'widget-prompt "S-TAB" (keymap:call complete! widget:target #t))
    (keymap:bind-default! 'widget-prompt "C-g" (keymap:call cancel! widget:target))))
