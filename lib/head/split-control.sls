;; Backend splitter geometry shared by managers and other compositions.
(import (only (foundation edoc) elibrary))
(elibrary (head split-control)
  (export definition drag! init! resize!)
  (import (chezscheme) (prefix (head head) head:)
    (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
    (prefix (head layout) layout:) (prefix (head widget) widget:)
    (prefix (state view) view:))
  (define horizontal (layout:container 'x))
  (define vertical (layout:container 'y))
  (define dragging (make-hashtable equal-hash equal?))
  (define (horizontal? d) (eq? (cdr (assq 'axis (view:options d))) 'x))
  (define (split-layout d width height measure locate)
    (let* ([across? (horizontal? d)]
           [sizes (layout:linear (if across? width height) (if across? 1 0)
                    (map (lambda (n) (list 0 0 (list 'grow n))) (list-head (view:state d) (length (view:children d)))))])
      (map (lambda (child size)
             (list (cadr child) (if across? (list (car size) 0 (cadr size) height)
                                  (list 0 (car size) width (cadr size))))) (view:children d) sizes)))
  (define (divider d width height)
    (let ([parts (split-layout d width height #f #f)])
      (and (= (length parts) 2)
        (let* ([rect (cadr (car parts))]
               [at (if (horizontal? d) (caddr rect) (- (cadddr rect) 1))])
          (and (<= 0 at) (< at (if (horizontal? d) width height)) at)))))
  (define (split-render data d width height range)
    (let ([at (divider d width height)])
      (map (lambda (y)
             (let ([line (make-string width #\space)])
               (when at
                 (if (horizontal? d) (string-set! line at #\│)
                   (when (= y at) (string-fill! line #\─)))) line))
        (map (lambda (n) (+ n (car range))) (iota (cdr range))))))
  (define (divider? f x y)
    (let* ([d (widget:frame-descriptor f)] [rect (widget:frame-rect f)]
           [at (divider d (caddr rect) (cadddr rect))])
      (and at (= at (if (horizontal? d) x y)))))
  (define (split-bindings f x y)
    (if (divider? f x y) (list (list '(drag primary ()) (keymap:call drag! (widget:frame-id f)))) '()))

  (edoc "Set a mounted split's logical proportions immediately. The displayed child identities guard the operation; publication coalesces through the normal interaction channel, without a synchronous request on each pointer movement."
        (id model "split") (expected list "two displayed child references")
        (weights list "two positive rational proportions"))
  (define (resize! id expected weights)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (unless (and (memq (view:kind d) '(window-split split)) (equal? expected (map cadr (view:children d)))
                (list? weights) (= (length weights) 2) (for-all (lambda (n) (and (rational? n) (> n 0))) weights))
        (error 'resize! "split changed or proportions are invalid"))
      (interaction:set-state! head:ui-actor id #f weights)))

  (edoc "Begin resizing the shown split divider. Pointer movement supplies backend geometry; only logical proportions are published. A changed topology or cancelled capture ends the gesture."
        (id model "split under the pointer"))
  (define (drag! id)
    (let ([f (widget:event-frame)])
      (unless (and f (equal? id (widget:frame-id f))) (error 'drag! "expected a shown pointer target"))
      (hashtable-set! dragging id (map cadr (view:children (widget:frame-descriptor f))))
      (widget:capture! id)))
  (define (split-event! id source d event)
    (case (car event)
      [(cancel blur) (hashtable-delete! dragging id)]
      [(pointer)
       (let* ([f (widget:event-frame)] [phase (cadr event)] [expected (hashtable-ref dragging id #f)])
         (cond
           [(and (eq? phase 'press) (eq? (caddr event) 'primary)
              (divider? f (list-ref event 4) (list-ref event 5))) (drag! id) #t]
           [(and expected (memq phase '(move release)))
            (let* ([rect (widget:frame-rect f)] [extent (if (horizontal? d) (- (caddr rect) 1) (cadddr rect))]
                   [at (min (- extent 1) (max 1 (+ (list-ref event (if (horizontal? d) 4 5)) (if (horizontal? d) 0 1))))])
              (when (>= extent 2) (resize! id expected (list at (- extent at))))
              (when (eq? phase 'release) (hashtable-delete! dragging id)) #t)]
           [else #f]))]
      [else #f]))

  (edoc "Reusable two-part splitter presentation. The axis option is x or y; state holds two positive proportions. One attached child fills the allocation without losing the saved proportions. Hidden children must be detached and explicitly retained by their owner." (returns list) (effects internal))
  (define (definition)
    (list (cons 'layout split-layout) (cons 'render split-render)
      (cons 'measure (lambda (data d axis cross measure)
                       ((cdr (assq 'measure (if (horizontal? d) horizontal vertical))) data d axis cross measure)))
      (cons 'pointer-bindings split-bindings) (cons 'event split-event!)
      (cons 'release (lambda (id) (hashtable-delete! dragging id)))
      (cons 'actions (list (cons 'resize resize!) (cons 'drag drag!)))))

  (edoc "Register an ordinary resizable splitter without constructing views." (public))
  (define (init!) (widget:register! 'split 1 (definition))))
