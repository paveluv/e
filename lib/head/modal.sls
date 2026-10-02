;; A composable placement host for ordinary prompt continuations.
(import (only (foundation edoc) elibrary))
(elibrary (head modal)
  (export create! init! prepare!)
  (import (chezscheme) (prefix (head head) head:)
          (prefix (head interaction) interaction:) (prefix (head widget) widget:)
          (prefix (state model) model:) (prefix (state view) view:))

  (define (get r key) (cdr (assq key r)))
  (define (root-of id)
    (let ([d (interaction:snapshot id)])
      (and d (if (view:parent d) (root-of (view:parent d)) id))))
  (define (within? id parent)
    (and id (or (equal? id parent)
              (let ([d (interaction:snapshot id)]) (and d (within? (view:parent d) parent))))))
  (define (mounted? id)
    (guard (ex [else #f])
      (let-values ([(source d inputs) (widget:context id 'current)])
        (equal? (view:owner d) head:ui-actor))))
  (define (arrange! id children)
    (interaction:flush!)
    (let ([r (caddar (cadr (model:snapshots (list id))))])
      (let-values ([(status rows)
                    (widget:arrange! (list (list id (get r 'revision) children (view:options (get r 'value)))))])
        (unless (eq? status 'applied) (error 'prepare! "prompt placement changed" status)))))

  (edoc "Create an empty prompt host under an explicit lifetime. Bind a containing view's prompt command to this host's prepare action. Delegates name ancestor receivers and their allowed keymaps; the host has no input loop or implicit editor bindings."
        (owner (or model #f) "lifetime owner") (delegates list "(ancestor-model context ...) rows allowed while prompting") (returns model))
  (define (create! owner delegates)
    (unless (and (list? delegates) (for-all (lambda (r) (and (list? r) (pair? r) (model:reference? (car r))
                                                             (for-all symbol? (cdr r)))) delegates))
      (error 'create! "expected ancestor receivers and context names"))
    (view:create! head:ui-actor #f 'modal 1 (list (cons 'key-delegates delegates)) '() owner))

  (edoc "Capture a prompt origin in this mounted host. Return the parent request, portable origin and an attachment procedure. Attachment places a continuation as the top modal child and returns cleanup that restores surviving focus. Removing the host cancels its waiting callers through ordinary widget release."
        (receiver id (view modal)) (id model "prompt host") (returns (values any list procedure)))
  (define (prepare! id)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (unless (eq? (view:kind d) 'modal) (error 'prepare! "expected a modal host"))
      (let* ([root (root-of id)] [focus (widget:focused root)]
             [current (and focus (interaction:snapshot focus))]
             [children (view:children d)]
             [parent (and (pair? children) (view:source (interaction:snapshot (cadr (car (reverse children))))))]
             [origin (list (cons 'view focus) (cons 'generation (and current (view:generation current)))
                       (cons 'receivers (if current (widget:receivers focus) '()))
                       (cons 'focus focus) (cons 'source (and current (view:source current))))])
        (values parent origin
          (lambda (request receiver)
            (unless (and (mounted? id) (equal? root (root-of id))
                      (equal? children (view:children (interaction:snapshot id))))
              (error 'prepare! "prompt host changed before attachment"))
            (let* ([r (caddar (cadr (model:snapshots (list receiver))))] [options (view:options (get r 'value))]
                   [delegates (get (view:options d) 'key-delegates)])
              (let-values ([(status rows)
                            (view:arrange! head:ui-actor
                              (list (list receiver (get r 'revision) (view:children (get r 'value))
                                      (cons (cons 'key-delegates delegates) (remp (lambda (p) (eq? (car p) 'key-delegates)) options)))) '())])
                (unless (eq? status 'applied) (error 'prepare! "prompt receiver changed" status))))
            (arrange! id (append children (list (list (string->symbol (format "request-~a" (cadr request))) receiver 'fit))))
            (interaction:focus! root receiver)
            (lambda ()
              (when (mounted? id)
                (let* ([children (view:children (interaction:snapshot id))]
                       [remaining (remp (lambda (c) (equal? (cadr c) receiver)) children)]
                       [restore? (within? (widget:focused root) receiver)])
                  (unless (equal? children remaining)
                    (arrange! id remaining)
                    (when (and restore? focus (within? focus root))
                      (interaction:focus! root focus)))))))))))

  (edoc "Register the modal prompt host. Only its top child is visible; an empty host occupies no space. Loading creates no root or prompt request." (public))
  (define (init!)
    (widget:register! 'modal 1
      (list
        (cons 'measure (lambda (data d axis cross measure)
                         (if (null? (view:children d)) '(0 0)
                           (measure (cadr (car (reverse (view:children d)))) axis cross))))
        (cons 'layout (lambda (d width height measure locate)
                        (map (lambda (child) (list (cadr child)
                                               (list 0 0 width (if (eq? child (car (reverse (view:children d)))) height 0)))) (view:children d))))
        (cons 'actions (list (cons 'prepare prepare!))))))
)
