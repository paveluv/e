;; Prompt controls own no keyboard reader or window. The host places the tree.
(import (only (foundation edoc) elibrary))
(elibrary (head prompt-control)
  (export accept! cancel! create! drain! init!)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (head head) head:) (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:) (prefix (head layout) layout:)
          (prefix (head suspension) suspension:) (prefix (head text-source) text-source:)
          (prefix (head widget) widget:) (prefix (service log) log:)
          (prefix (service prompt-request) prompt-request:) (prefix (state model) model:)
          (prefix (state view) view:))

  (define (get r key) (cdr (assq key r)))
  (define live (make-hashtable equal-hash equal?))
  (define pending '())
  (define-record-type runtime (fields request generation (mutable delivered?) (mutable closed?)))
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
        (hashtable-delete! live id)
        ;; A host may be walking the very descriptors it is releasing.
        ;; Teardown runs after unmount/reconciliation has finished.
        (queue! (lambda () (prompt-request:close! head:ui-actor (runtime-request r)))))))

  (define (service! id frame)
    (guard (ex [else (release! id)])
      (let-values ([(source d value) (context id)])
        (let* ([request (get source 'id)] [generation (view:generation d)]
               [status (get value 'status)]
               [r (and (equal? id (get value 'controller))
                    (or (hashtable-ref live id #f)
                      (let ([r (make-runtime request generation (not (eq? status 'editing)) #f)])
                        (hashtable-set! live id r) r)))])
          ;; The base identifies the controller, independent of mount order.
          ;; Mounting a terminal outcome never replays it, including reload.
          (unless (or (not r) (runtime-delivered? r) (runtime-closed? r) (eq? status 'editing))
            (runtime-delivered?-set! r #t)
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
                 [help (create-view! who #f 'label 1
                         (list (cons 'text (cond [(assq 'help options) => cdr] [else ""])) '(role . ghost)) '() request)])
            (let-values ([(status rows)
                          (view:arrange! who
                            (list (list input 0 (list (list 'label label 'fit) (list 'entry entry '(grow 1))) '((spacing . normal)))
                              (list root 0 (list (list 'help help 'fit) (list 'input input '(grow 1))) (list (cons 'commands commands)))) '())])
              (unless (eq? status 'applied) (error 'create! "cannot compose prompt" status)))
            (let ([status (prompt-request:bind! who request (get r 'revision) root)])
              (unless (eq? status 'applied) (error 'create! "request controller changed" status))) root)))))

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
    (widget:register! 'prompt 1
      (append (layout:container 'y)
        (list (cons 'capture-contexts '(widget-prompt)) (cons 'service service!) (cons 'release release!)
          (cons 'actions (list (cons 'accept accept!) (cons 'cancel cancel!))))))
    (keymap:bind-default! 'widget-prompt "RET" (keymap:call accept! widget:target))
    (keymap:bind-default! 'widget-prompt "C-g" (keymap:call cancel! widget:target))))
