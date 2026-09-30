;; A process capture parent around the ordinary read-only editor viewport.
(import (only (foundation edoc) elibrary))
(elibrary (head terminal-control)
  (export (rename (terminal-state:create! create-view!)) follow! forward-clipboard-to-copy-buffer page! paste! pointer! press! register! send! set-capture! toggle-capture!)
  (import (chezscheme) (prefix (core kernel) kernel:)
    (prefix (head editor) editor:) (prefix (head head) head:)
    (prefix (head interaction) interaction:) (prefix (head keymap) keymap:) (prefix (head render) render:)
    (prefix (head terminal-state) terminal-state:) (prefix (head text-source) text-source:)
    (prefix (head widget) widget:) (prefix (service log) log:)
    (prefix (state actor) actor:) (prefix (state store) store:) (prefix (state view) view:))

  (define-record-type mount (fields (mutable facts) (mutable offered) (mutable pointer) toggle))
  (define mounts (make-hashtable equal-hash equal?))
  (define (mounted id)
    (or (hashtable-ref mounts id #f)
      (let ([m (make-mount '() #f #f (keymap:call toggle-capture! id))]) (hashtable-set! mounts id m) m)))
  (define (fact facts key fallback) (cond [(assq key facts) => cdr] [else fallback]))

  (edoc "Whether terminal OSC 52 clipboard requests are forwarded through the installed clipboard capability."
        (value boolean))
  (define forward-clipboard-to-copy-buffer
    (make-parameter #t (lambda (enabled?)
                         (unless (boolean? enabled?) (error 'forward-clipboard-to-copy-buffer "expected a boolean")) enabled?)))
  (define copy-text! #f)
  (define presented (unbox (kernel:persistent-cell 'terminal-presented-sources (lambda () (make-weak-eq-hashtable)))))
  (define (present-notices! document facts)
    (let* ([source (text-source:lookup document)] [old (hashtable-ref presented source '(0))]
           [clipboard (fact facts 'clipboard #f)] [diagnostics (fact facts 'diagnostics '())]
           [sequence (if clipboard (car clipboard) (car old))])
      ;; Claim before callbacks. Views share this source identity; output is
      ;; delivered once even through reentrant pumps or multiple placements.
      (hashtable-set! presented source (cons sequence diagnostics))
      (when (and clipboard (> sequence (car old)) (equal? (cadr clipboard) head:ui-actor)
              (forward-clipboard-to-copy-buffer) copy-text!)
        (copy-text! (caddr clipboard))
        (log:add! 'terminal-control:present-notices! (format "Copied clipboard text from ~a" (store:buffer-name document))))
      (for-each (lambda (message)
                  (unless (member message (cdr old))
                    (log:add! 'terminal-control:present-notices! (format "~a: ~a" (store:buffer-name document) message)))) diagnostics)))
  (define (refuse message) (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (context id)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (unless (and d (eq? (view:kind d) 'terminal) (= (view:schema d) 1)
                (view:source d) (eq? (car (view:source d)) 'buffer))
        (error 'terminal "expected a mounted terminal view" id))
      (values (cadr (view:source d)) d)))
  (define (live? id) (fact (mount-facts (mounted id)) 'alive #f))
  (define (child id) (widget:descendant id 'text))
  (define (size id)
    (let ([f (widget:prepared id)])
      (and f (> (caddr (widget:frame-rect f)) 0) (> (cadddr (widget:frame-rect f)) 0)
        (list (cadddr (widget:frame-rect f)) (caddr (widget:frame-rect f))))))
  (define (witness id d) (list id (view:generation d)))

  (edoc "Choose partial capture (yield C-x and M-x) or full capture for an explicit live terminal view."
        (receiver id (view terminal)) (id model "terminal view") (capture (one-of partial full) "input policy"))
  (define (set-capture! id capture)
    (unless (memq capture '(partial full)) (error 'set-capture! "expected partial or full"))
    (let-values ([(document d) (context id)])
      (unless (live? id) (refuse "The terminal process has exited"))
      (interaction:set-state! head:ui-actor id #f (list capture (cadr (terminal-state:state d))))))

  (edoc "Toggle partial/full capture for an explicit live terminal view."
        (receiver id (view terminal)) (id model "terminal view"))
  (define (toggle-capture! id)
    (let-values ([(document d) (context id)])
      (set-capture! id (if (eq? (car (terminal-state:state d)) 'full) 'partial 'full))))

  (edoc "Follow the process cursor or retain the shown selection and scrollback position. Following derives its cursor from base output without publishing one interaction per output frame."
        (receiver id (view terminal)) (id model "terminal view") (following? boolean "whether to follow"))
  (define (follow! id following?)
    (unless (boolean? following?) (error 'follow! "expected a boolean"))
    (when (and following? (not (live? id))) (refuse "The terminal process has exited"))
    (let-values ([(document d) (context id)])
      (let ([state (terminal-state:state d)])
        (unless (eq? following? (cadr state))
          (unless following?
            (let* ([text (child id)] [f (widget:prepared text)] [state (and f (editor:frame-state f))])
              (when state (interaction:set-state! head:ui-actor text
                            (cdr (assq 'revision (widget:frame-source f))) state))))
          (interaction:set-state! head:ui-actor id #f (list (car state) following?))))))

  (define (emit! id event data)
    (let-values ([(document d) (context id)])
      (let* ([m (mounted id)] [grid (size id)] [lease (witness id d)])
        (unless (and grid (live? id)) (refuse "Terminal input requires an allocated live view"))
        (unless (member event '("FOCUS" "BLUR")) (follow! id #t))
        ;; Input carries the current size. Suppress a second resize offer for
        ;; the same allocation when the service pump next runs.
        (unless (member event '("FOCUS" "BLUR")) (mount-offered-set! m (list lease grid)))
        (actor:send! (fact (mount-facts m) 'app #f)
          (list 'input head:ui-actor document event
            (append data (list (cons 'view lease) (cons 'size grid) (cons 'color-scheme (head:host-color-scheme)))))))))

  (edoc "Type text into the process displayed by an explicit terminal view."
        (receiver id (view terminal)) (id model "terminal view") (text string "typed text"))
  (define (send! id text)
    (unless (string? text) (error 'send! "expected text")) (emit! id "TEXT" (list (cons 'text text))))

  (edoc "Paste text into a terminal process, honoring its bracketed-paste mode."
        (receiver id (view terminal)) (id model "terminal view") (text string "pasted text"))
  (define (paste! id text)
    (unless (string? text) (error 'paste! "expected text")) (emit! id "PASTE" (list (cons 'paste text))))

  (edoc "Send a normalized key event to a terminal process."
        (receiver id (view terminal)) (id model "terminal view") (key string "key token, for example C-x or UP"))
  (define (press! id key)
    (unless (string? key) (error 'press! "expected a key token")) (emit! id key '()))

  (edoc "Send a process pointer event against a displayed VT grid. The base refuses stale revisions, generations and out-of-grid coordinates."
        (receiver id (view terminal)) (id model "terminal view") (event string "normalized VT pointer token")
        (address list "(text-revision surface-generation row column button-code)"))
  (define (pointer! id event address)
    (unless (and (member event '("MOUSE-CLICK" "MOUSE-DRAG" "MOUSE-RELEASE" "WHEEL-UP" "WHEEL-DOWN" "WHEEL-LEFT" "WHEEL-RIGHT"))
              (list? address) (= (length address) 5) (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0))) address)
              (<= (list-ref address 4) 223))
      (error 'pointer! "expected a process pointer event and frame address"))
    (cond [(string=? event "MOUSE-RELEASE") (mount-pointer-set! (mounted id) #f)]
      [(member event '("MOUSE-CLICK" "MOUSE-DRAG")) (mount-pointer-set! (mounted id) address)])
    (emit! id event (list (cons 'revision (car address)) (cons 'generation (cadr address))
                      (cons 'cell (cons (caddr address) (cadddr address))) (cons 'button (list-ref address 4)))))

  (edoc "Page an explicit terminal's read-only viewport, leaving process-cursor following."
        (receiver id (view terminal)) (id model "terminal view") (direction integer "-1 up or 1 down"))
  (define (page! id direction) (follow! id #f) (editor:page! (child id) direction 1))

  (define (captured? id event)
    (and (live? id)
      (let ([capture (fact (mount-facts (mounted id)) 'capture #f)])
        (or (eq? capture 'all)
          (and (pair? capture) (eq? (car capture) 'except) (not (member event (cdr capture))))))))
  (define (pointer-address frame x y button)
    (let* ([text (and (pair? (widget:frame-children frame)) (car (widget:frame-children frame)))]
           [row (and text (<= 0 x) (< x (caddr (widget:frame-rect text))) (<= 0 y)
                  (< y (cadddr (widget:frame-rect text))) (editor:frame-row text y))]
           [header (and row (render:header (caddr row)))])
      (and header (list (cadr header) (car header) (car row) (+ (list-ref row 3) x) button))))

  (define (capture-event! id source d event)
    (and (live? id)
      (case (car event)
        [(key) (press! id (cadr event)) #t]
        [(text) ((if (eq? (caddr event) 'paste) paste! send!) id (cadr event)) #t]
        [(scroll)
         (let* ([token (if (< (caddr event) 0) "WHEEL-UP" "WHEEL-DOWN")]
                [address (and (captured? id token) (pointer-address (widget:event-frame) (list-ref event 4) (list-ref event 5)
                                                     (if (< (caddr event) 0) 64 65)))])
           (if address (begin (pointer! id token address) #t) (begin (follow! id #f) #f)))]
        [(pointer)
         (let* ([phase (cadr event)] [button (caddr event)] [mods (cadddr event)]
                [token (case phase [(press) "MOUSE-CLICK"] [(release) "MOUSE-RELEASE"] [else "MOUSE-DRAG"])]
                [code (case button [(primary) 0] [(middle) 1] [(secondary) 2] [else #f])]
                [code (and code (+ code (if (eq? phase 'move) 32 0) (if (memq 'control mods) 16 0) (if (memq 'alt mods) 8 0)))]
                [address (or (and code (not (memq 'shift mods)) (captured? id token)
                               (pointer-address (widget:event-frame) (list-ref event 4) (list-ref event 5) code))
                           (and (eq? phase 'release) (mount-pointer (mounted id))))])
           (cond [address (pointer! id token address) (when (eq? phase 'press) (widget:capture! id)) #t]
             [else (when (eq? phase 'press) (follow! id #f)) #f]))]
        [else #f])))
  (define (event! id source d event)
    (case (car event)
      [(focus blur) (when (and (live? id) (size id)) (press! id (if (eq? (car event) 'focus) "FOCUS" "BLUR"))) #f]
      [(cancel)
       (let ([address (mount-pointer (mounted id))])
         (mount-pointer-set! (mounted id) #f)
         (when (and address (live? id) (size id)) (pointer! id "MOUSE-RELEASE" address))) #t]
      [else #f]))
  (define (pointer-bindings frame x y)
    (let* ([id (widget:frame-id frame)] [address (and (captured? id "MOUSE-CLICK") (pointer-address frame x y 0))])
      (if address (list (list '(click primary ()) (keymap:call pointer! id "MOUSE-CLICK" address))) '())))
  (define (status id d active?)
    (let* ([facts (mount-facts (mounted id))]
           [text (case (fact facts 'process-state 'starting)
                   [(running) (if (fact facts 'bell #f) "♪" "▶")]
                   [(failed) "■ error"] [(stopped) "■"] [else "starting"])])
      (append (list (cons (string-append " " text) #f))
        (if (live? id)
          (list (cons " " #f) (cons (if (eq? (car (terminal-state:state d)) 'full) "●" "◐") (mount-toggle (mounted id)))) '()))))
  (define (service! id frame)
    (let* ([d (interaction:snapshot id)] [source (and d (view:source d))])
      (when (and source (eq? (car source) 'buffer) (store:visible? head:ui-actor (cadr source)))
        (let* ([m (mounted id)] [facts (store:properties (cadr source))]
               [alive? (fact facts 'alive #f)] [grid (size id)] [lease (witness id d)] [offer (list lease grid)])
          (mount-facts-set! m facts)
          (when (text-source:lookup (cadr source)) (present-notices! (cadr source) facts))
          (when (and (not alive?) (cadr (terminal-state:state d))) (follow! id #f))
          (when (and alive? grid (not (equal? offer (mount-offered m))))
            (mount-offered-set! m offer)
            (actor:send! (fact facts 'app #f) (list 'request head:ui-actor (cadr source) 'resize
                                                (list (cons 'view lease) (cons 'size grid)))))))))

  (edoc "Install terminal capture around the shared editor viewport; acquisition, notices and resize offers run on the service path."
        (clipboard (or procedure #f) "text -> unspecified; optional host clipboard capability"))
  (define (register! clipboard)
    (set! copy-text! clipboard)
    (widget:register! 'terminal 1
      (list (cons 'service service!) (cons 'status status) (cons 'release (lambda (id) (hashtable-delete! mounts id)))
        (cons 'capture (lambda (id d) (if (live? id) 'full 'partial)))
        (cons 'capture-contexts (lambda (id d) (if (live? id) '(terminal-capture) '())))
        (cons 'yield (lambda (id d) (if (and (live? id) (eq? (car (terminal-state:state d)) 'partial)) '("C-x" "M-x") '())))
        (cons 'capture-event capture-event!) (cons 'event event!) (cons 'capture-pointer-bindings pointer-bindings)
        (cons 'layout (lambda (d width height measure locate) (list (list (cadar (view:children d)) (list 0 0 width height)))))
        (cons 'actions (list (cons 'capture set-capture!) (cons 'follow follow!) (cons 'send send!)))))
    (keymap:bind-default! 'terminal-capture "C-]" (keymap:call toggle-capture! widget:target))
    (keymap:bind-default! 'terminal-capture "S-PGUP" (keymap:call page! widget:target -1))
    (keymap:bind-default! 'terminal-capture "S-PGDN" (keymap:call page! widget:target 1)))
)
