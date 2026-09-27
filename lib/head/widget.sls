;; Definition lookup and a temporary window adapter. Models and descriptors
;; remain in the base; mounts, geometry and rendering caches belong to a head.
(import (only (foundation edoc) elibrary))
(elibrary (head widget)
  (export act! actions init! mount! register! unmount!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:)
          (prefix (foundation string) string:)
          (prefix (head echo) echo:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (state model) model:)
          (prefix (sys glyph) glyph:))

  (define definitions (kernel:make-registry car))
  (define mounts (make-hashtable equal-hash equal?))
  (define-record-type mount (fields id model owner buffer (mutable mirrored) (mutable rendered)))
  (define (definition descriptor)
    (kernel:registry-find definitions
      (lambda (entry) (equal? (car entry) (list (cadr descriptor) (caddr descriptor))))))
  (define (mounted id)
    (or (hashtable-ref mounts id #f) (error 'widget "view is not mounted" id)))

  (edoc "Register a widget renderer and public actions, owned by the defining module. Retraction leaves mounts as placeholders."
        (kind symbol "renderer kind") (schema integer "positive schema version")
        (render procedure "(render model-envelope interaction-state width height) returns display lines; the host clips to geometry")
        (actions list "unique (name . procedure) pairs; each receives view-id, model-envelope, provisional descriptor and action arguments; optional input handles event, point and size"))
  (define (register! kind schema render actions)
    (unless (and (symbol? kind) (integer? schema) (exact? schema) (> schema 0) (procedure? render)
                 (list? actions) (for-all (lambda (entry) (and (pair? entry) (symbol? (car entry)) (procedure? (cdr entry)))) actions)
                 (let unique ([rest actions] [seen '()])
                   (or (null? rest) (and (not (memq (caar rest) seen)) (unique (cdr rest) (cons (caar rest) seen))))))
      (error 'register! "expected renderer kind/schema, procedure and unique named actions"))
    (kernel:registry-add! definitions (list (list kind schema) render (map (lambda (entry) (cons (car entry) (cdr entry))) actions))))

  (edoc "List the available actions for a mounted view; unavailable definitions or models have no actions."
        (id list "view id") (returns list))
  (define (actions id)
    (mounted id)
    (let* ([descriptor (interaction:snapshot id)] [entry (and descriptor (definition descriptor))])
      (if (and entry (model:available? (car descriptor))) (map car (caddr entry)) '())))

  (edoc "Operate a mounted widget through its public action, with the current model snapshot and actual provisional target/basis."
        (id list "view id") (action symbol "action name") (arguments (list-of any) "action arguments") (returns any))
  (define (act! id action . arguments)
    (mounted id)
    (let* ([descriptor (interaction:snapshot id)] [entry (and descriptor (definition descriptor))]
           [procedure (and entry (assq action (caddr entry)))])
      (unless (and procedure (model:available? (car descriptor))) (error 'act! "widget action is unavailable" id action))
      (call-with-values (lambda () (apply (cdr procedure) id (model:snapshot (car descriptor)) descriptor arguments))
        (lambda result
          (when (or (not (eq? action 'input)) (exists values result)) (head:wake-main!))
          (apply values result)))))

  (define (bounded lines width height)
    (unless (and (list? lines) (for-all string? lines)) (error 'widget "renderer must return a list of lines"))
    (let ([lines (list-head lines (min height (length lines)))])
      (map (lambda (line) (glyph:fit line width)) (if (null? lines) '("") lines))))
  (define (refresh! mount)
    (let* ([id (mount-id mount)] [b (mount-buffer mount)] [descriptor (interaction:snapshot id)]
           [entry (and descriptor (definition descriptor))]
           [mirrored (or (mount-mirrored mount)
                         (let ([value (cons (model:available? (mount-model mount)) (model:snapshot (mount-model mount)))])
                           (mount-mirrored-set! mount value) value))]
           [model (cdr mirrored)] [available? (car mirrored)]
           [windows (filter (lambda (w) (eq? (head:window-buffer w) b)) (head:windows))]
           [geometry (map (lambda (w) (list w (max 1 (head:window-content-width w)) (max 1 (head:window-size w)))) windows)]
           [key (list entry model available? descriptor geometry)])
      (unless (equal? key (mount-rendered mount))
        (let* ([presentations
                (map (lambda (size)
                       (let ([width (cadr size)] [height (caddr size)])
                         (cons (car size)
                           (bounded
                             (if (and entry available?)
                                 ((cadr entry) (datum:copy model) (datum:copy (list-ref descriptor 7)) width height)
                                 (list (if descriptor
                                           (format "[Unavailable widget ~a/~a; model ~s]"
                                             (cadr descriptor) (caddr descriptor) (car descriptor))
                                           (format "[Unavailable view ~s; inspect with model:snapshot]" id)))) width height)))) geometry)]
               [count (apply max 1 (map (lambda (entry) (length (cdr entry))) presentations))]
               [padded (map (lambda (entry) (cons (car entry) (append (cdr entry) (make-list (- count (length (cdr entry))) "")))) presentations)])
          (head:view-replace! b (if (null? padded) '("") (cdar padded)) '()
            (apply append (map (lambda (w) (list (cons w '(0 . 0)) (cons (cons 'top w) '(0 . 0)))) windows)) padded)
          (mount-rendered-set! mount key)))))

  (edoc "Mount a base view and return its local adapter buffer. Showing that buffer is the host's choice. A second call in this head reuses the mount."
        (id list "view model id") (returns buffer))
  (define (mount! id)
    (let ([old (hashtable-ref mounts id #f)])
      (if old (mount-buffer old)
          (kernel:call-with-runtime-registrations
            (lambda ()
              (let-values ([(status descriptor) (interaction:claim! head:ui-actor id)])
                (unless (memq status '(applied unavailable)) (error 'mount! "view cannot be mounted" status id))
                (unless (eq? status 'applied) (set! descriptor #f))
                (let* ([owner (gensym "widget-mount")]
                       [source (if descriptor (car descriptor) id)]
                       [b (head:new-local-buffer! (format "widget ~a" (cadr id)))]
                       [mount (make-mount (datum:copy id) (datum:copy source) owner b #f #f)])
                  (guard (ex [else
                              (when descriptor
                                (guard (ignored [else (void)]) (interaction:release! head:ui-actor id (cadddr descriptor))))
                              (kernel:retract-module! owner)
                              (hashtable-delete! mounts id) (head:forget-buffer! b) (raise ex)])
                    (parameterize ([kernel:registering-module owner])
                      (model:subscribe! (list source)
                        (lambda (notice) (mount-mirrored-set! mount #f) (head:wake-main!)))
                      (head:register-app! b (lambda () (refresh! mount))
                        (lambda (event)
                          (and (memq 'input (actions id))
                               (act! id 'input event (head:app-event-buffer-position)
                                 (list (head:window-content-width (head:current-window))
                                       (head:window-size (head:current-window))))))))
                    (hashtable-set! mounts (datum:copy id) mount)
                    (head:buffer-fact-set! b 'resume-kind 'widget)
                    (head:buffer-fact-set! b 'widget-id (datum:copy id))
                    (head:set-app-presentation! b 0 #f #f)
                    (head:set-app-cursor-visible! b #f)
                    (head:set-app-selectable! b #f)
                    (head:set-app-manages-viewport! b #t)
                    (head:set-app-status-position! b (lambda (b) "")) b))))))))

  (edoc "Unmount a view, releasing subscriptions and owner state after acknowledgement; its model and persistent descriptor remain."
        (id list "view id"))
  (define (unmount! id)
    (let ([mount (hashtable-ref mounts id #f)])
      (when mount
        (let ([descriptor (interaction:snapshot id)])
          (dynamic-wind void
            (lambda () (when descriptor (interaction:release! head:ui-actor id (cadddr descriptor))))
            (lambda ()
              (hashtable-delete! mounts id)
              (kernel:retract-module! (mount-owner mount))
              (head:forget-buffer! (mount-buffer mount))))))))

  ;; The minimal text widget deliberately consumes arbitrary model values.
  ;; It needs no extra base dataset service or evaluator allocation.
  (define (text-lines model)
    (let ([value (cdr (assq 'value model))])
      (string:lines (if (string? value) value (format "~s" value)))))
  (define (text-state state count)
    (map (lambda (n) (if (and (integer? n) (exact? n)) (max 0 (min (- count 1) n)) 0))
      (if (and (list? state) (= (length state) 2)) state '(0 0))))
  (define (text-render model state width height)
    (let* ([lines (text-lines model)] [state (text-state state (length lines))] [top (cadr state)])
      (map (lambda (line row) (string-append (if (= row (car state)) "> " "  ") line))
        (list-head (list-tail lines top) (min height (- (length lines) top)))
        (map (lambda (n) (+ top n)) (iota (min height (- (length lines) top)))))))
  (define (text-move! id model descriptor offset height)
    (let* ([count (length (text-lines model))] [state (text-state (list-ref descriptor 7) count)]
           [row (max 0 (min (- count 1) (+ (car state) offset)))]
           [top (min row (max (cadr state) (- row (max 1 height) -1)))])
      (interaction:set-state! head:ui-actor id (cdr (assq 'revision model)) (list row top)) (void)))
  (define (text-scroll! id model descriptor offset)
    (let* ([count (length (text-lines model))] [state (text-state (list-ref descriptor 7) count)])
      (interaction:set-state! head:ui-actor id (cdr (assq 'revision model))
        (list (car state) (max 0 (min (- count 1) (+ (cadr state) offset))))) (void)))
  (define (text-select! id model descriptor row)
    (let* ([count (length (text-lines model))] [state (text-state (list-ref descriptor 7) count)])
      (interaction:set-state! head:ui-actor id (cdr (assq 'revision model))
        (list (max 0 (min (- count 1) (+ (cadr state) row))) (cadr state)))) (void))
  (define (text-choose! id model descriptor)
    (let* ([lines (text-lines model)] [row (car (text-state (list-ref descriptor 7) (length lines)))]
           [chosen (list-ref lines row)])
      (echo:set-text! chosen)
      (list (car descriptor) (cdr (assq 'revision model)) row chosen)))
  (define (text-input! id model descriptor event point size)
    (cond [(member event '("UP" "DOWN")) (act! id 'move (if (string=? event "UP") -1 1) (cadr size)) #t]
          [(member event '("WHEEL-UP" "WHEEL-DOWN")) (act! id 'scroll (if (string=? event "WHEEL-UP") -3 3)) #t]
          [(string=? event "MOUSE-CLICK") (when point (act! id 'select (car point))) 'keep-focus]
          [(string=? event "RET") (act! id 'choose) #t]
          [else #f]))

  (edoc "Install the text widget, model-aware checkpoint restore and mount cleanup. Renderer definitions may be supplied by extensions.")
  (define (init!)
    (register! 'text 1 text-render
      (list (cons 'input text-input!) (cons 'move text-move!) (cons 'scroll text-scroll!)
            (cons 'select text-select!) (cons 'choose text-choose!)))
    (kernel:registry-observe! definitions (lambda (removed added) (head:wake-main!)))
    (head:add-buffer-kill-hook!
      (lambda (b) (let ([id (head:buffer-fact b 'widget-id #f)]) (when id (unmount! id)))))
    (head:register-resume! 'widget
      (lambda (b positions) (values (list (head:buffer-fact b 'widget-id #f)) '()))
      (lambda (reference positions) (values (mount! (car reference)) '())))))
