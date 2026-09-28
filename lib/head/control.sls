;; Small composable controls. Authored text remains in the existing entry.
(import (only (foundation edoc) elibrary))
(elibrary (head control)
  (export activate! create-filter! init!)
  (import (chezscheme) (prefix (head keymap) keymap:) (prefix (head layout) layout:)
          (prefix (head widget) widget:) (prefix (state view) view:)
          (prefix (sys glyph) glyph:))
  (define hover (make-hashtable equal-hash equal?))
  (define held (make-hashtable equal-hash equal?))
  (define (input inputs name fallback)
    (let ([p (assq name inputs)]) (if (and p (eq? (cadr p) 'ready)) (caddr p) fallback)))
  (define (data id source inputs) (list id (input inputs 'text "[Unavailable]") (input inputs 'enabled #f)))
  (define (enabled? id)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (and (input inputs 'enabled #f) (assq 'activate (widget:commands id)) #t)))

  (edoc "Invoke an enabled action-text control's explicit command target." (id list "action view") (returns any))
  (define (activate! id)
    (unless (enabled? id) (error 'activate! "action is unavailable" id))
    (widget:invoke! id 'activate))

  (define (render data d width height range)
    (if (zero? (car range)) (list (glyph:fit (cadr data) width)) '()))
  (define (measure data d axis cross child)
    (if (eq? axis 'y) '(1 1) (list 0 (glyph:cells (cadr data)))))
  (define (decorate data d width height range)
    (let* ([id (car data)] [role (cond [(assq 'role (view:options d)) => cdr] [else #f])]
           [face (if (and (hashtable-ref hover id #f) (caddr data)) 'hover role)])
      (if (and face (zero? (car range))) (list (list (list 0 0 (min width (glyph:cells (cadr data))) 1) face)) '())))
  (define (inside? inputs event)
    (let* ([f (widget:event-frame)] [rect (widget:frame-rect f)] [clip (widget:frame-clip f)]
           [x (list-ref event 4)] [y (list-ref event 5)])
      (and (= y 0) (<= 0 x) (< x (glyph:cells (input inputs 'text "")))
        (layout:contains? clip (+ (car rect) x) (+ (cadr rect) y)))))
  (define (event! id source d event)
    (case (car event)
      [(cancel blur)
       (hashtable-delete! held id) (hashtable-delete! hover id) (widget:repaint! id)]
      [(pointer)
       (let-values ([(source d inputs) (widget:context id)])
         (let* ([phase (cadr event)] [inside (and (not (eq? phase 'leave)) (inside? inputs event))]
                [old (hashtable-ref hover id #f)])
           (if inside (hashtable-set! hover id #t) (hashtable-delete! hover id))
           (unless (eq? old inside) (widget:repaint! id))
           (case phase
             [(press) (when (and inside (eq? (caddr event) 'primary) (enabled? id))
                        (widget:capture! id) (hashtable-set! held id #t))]
             [(release)
              (let ([pressed (hashtable-ref held id #f)])
                (hashtable-delete! held id)
                (when (and pressed inside (enabled? id)) (activate! id)))])))]))

  (edoc "Compose a label, an existing single-line text entry and status text; the root exposes the entry's text output."
        (actor datum "creator") (source list "text buffer reference") (label string "label") (status string "status text") (returns list "root view"))
  (define (create-filter! actor source label status)
    (let* ([root (view:create! actor source 'filter 1 '((spacing . normal)) '())]
           [label (view:create! actor #f 'label 1 (list (cons 'text label)) '())]
           [entry (view:create! actor source 'entry 1 '() '((0 . 0) (0 . 0)))]
           [status (view:create! actor #f 'label 1 (list (cons 'text status) '(role . ghost)) '())])
      (view:arrange! actor (list (list root 0 (list (list 'label label 'fit) (list 'entry entry '(grow 1)) (list 'status status 'fit)) '((spacing . normal)))) '())
      root))

  (edoc "Install filter, label and action-text presentations and their public keyboard actions.")
  (define (init!)
    (widget:register! 'filter 1 (layout:container 'x))
    (widget:register! 'label 1 (list (cons 'prepare data) (cons 'render render) (cons 'measure measure) (cons 'decorate decorate)))
    (widget:register! 'action-text 1
      (list (cons 'prepare data) (cons 'render render) (cons 'measure measure) (cons 'decorate decorate)
        (cons 'focus #t) (cons 'contexts '(widget-action)) (cons 'event event!) (cons 'actions (list (cons 'activate activate!)))))
    (keymap:bind-default! 'widget-action "RET" (keymap:call activate! widget:target))
    (keymap:bind-default! 'widget-action "SPC" (keymap:call activate! widget:target))))
