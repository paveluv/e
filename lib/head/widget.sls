;; Recursive mounts are head runtime objects, independent of window buffers.
(import (only (foundation edoc) elibrary))
(elibrary (head widget)
  (export act! actions arrange! cancel! capture! event-frame focus! frame-children frame-clip frame-descriptor frame-id frame-lines frame-rect frame-source init! input! invalidate! key-scopes! mount! pointer! prepare! prepared present! register! reveal! shown target unmount!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:)
          (prefix (foundation string) string:)
          (prefix (head echo) echo:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:)
          (prefix (head layout) layout:)
          (prefix (state model) model:)
          (prefix (state view) view:)
          (prefix (sys glyph) glyph:))

  (define definitions (kernel:make-registry car))
  (define roots (make-hashtable equal-hash equal?))
  (define nodes (make-hashtable equal-hash equal?))
  (define-record-type mount (fields id slot (mutable subscription) (mutable ids)))
  (define-record-type node (fields id root (mutable mirrored) (mutable key) (mutable lines) (mutable data) (mutable data-key)))

  (edoc "An immutable prepared backend frame; only successful output makes it eligible for input."
        (id list "view id") (descriptor any "interaction basis") (definition any "definition identity")
        (source any "source basis") (rect list "absolute allocation within the root") (clip list "visible intersection")
        (children list "back-to-front child frames") (lines list "clipped backend output"))
  (define-record-type frame (fields id descriptor definition source rect clip children lines))
  (define preparations (make-hashtable equal-hash equal?))
  (define presentations '())
  (define (definition d)
    (and d (kernel:registry-find definitions
             (lambda (entry) (equal? (car entry) (list (view:kind d) (view:schema d)))))))
  (define (field entry name fallback)
    (cond [(and entry (assq name (cdr entry))) => cdr] [else fallback]))
  (define (mounted id)
    (or (hashtable-ref nodes id #f) (error 'widget "view is not mounted" id)))
  (define (rows id)
    (let walk ([id id])
      (let ([d (interaction:snapshot id)])
        (cons (cons id d) (if d (apply append (map (lambda (child) (walk (cadr child))) (view:children d))) '())))))

  (edoc "Register a head definition owned by the defining module; reject unknown and duplicate fields."
        (kind symbol "widget kind") (schema integer "positive version")
        (definition list "actions, contexts, focus, capture; optional render, measure, layout and event procedures"))
  (define (register! kind schema definition)
    (unless (and (symbol? kind) (integer? schema) (exact? schema) (> schema 0) (list? definition)
              (let loop ([rest definition] [seen '()])
                (or (null? rest)
                  (let ([p (car rest)])
                    (and (pair? p) (not (memq (car p) seen))
                      (case (car p)
                        [(actions) (and (list? (cdr p))
                                     (let check ([rest (cdr p)] [names '()])
                                       (or (null? rest)
                                         (and (pair? (car rest)) (symbol? (caar rest)) (procedure? (cdar rest))
                                           (not (memq (caar rest) names)) (check (cdr rest) (cons (caar rest) names))))))]
                        [(contexts capture-contexts) (and (list? (cdr p)) (for-all symbol? (cdr p)))]
                        [(yield) (and (list? (cdr p)) (for-all string? (cdr p)))]
                        [(focus) (boolean? (cdr p))]
                        [(capture) (memq (cdr p) '(full partial))]
                        [(prepare render measure layout event anchor locate) (procedure? (cdr p))]
                        [else #f])
                      (loop (cdr rest) (cons (car p) seen)))))))
      (error 'register! "invalid widget definition" kind schema definition))
    (kernel:registry-add! definitions (cons (list kind schema) (map (lambda (p) (cons (car p) (cdr p))) definition))))

  (define (source! n d)
    (let ([id (and d (view:source d))])
      (cond
        [(not d) (values #f #f)]
        [(not id) (values #t #f)]
        [(eq? (car id) 'buffer)
         (let ([b (head:buffer-of-store-id (cadr id))])
           (if b
             (let ([rev (head:content-revision b)])
               (unless (and (node-mirrored n) (= rev (caar (node-mirrored n))))
                 (node-mirrored-set! n
                   (cons (cons rev #t) (list (cons 'id id) (cons 'revision rev) (cons 'value (head:buffer-lines b))))))
               (values #t (cdr (node-mirrored n))))
             (values #f #f)))]
        [else
         (unless (node-mirrored n)
           (node-mirrored-set! n (cons (cons #f (model:available? id)) (model:snapshot id))))
         (values (cdar (node-mirrored n)) (cdr (node-mirrored n)))])))

  (edoc "List available named actions of a mounted view, including a nested child."
        (id list "view id") (returns list) (effects internal))
  (define (actions id)
    (let* ([n (mounted id)] [d (interaction:snapshot id)] [entry (definition d)])
      (let-values ([(available? source) (source! n d)])
        (if available? (map car (field entry 'actions '())) '()))))

  (edoc "Invoke a named action with an explicit view, coherent source and provisional interaction."
        (id list "view id") (action symbol "action") (arguments (list-of any) "action arguments") (returns any))
  (define (act! id action . arguments)
    (let* ([n (mounted id)] [d (interaction:snapshot id)] [entry (definition d)]
           [proc (assq action (field entry 'actions '()))])
      (let-values ([(available? source) (source! n d)])
        (unless (and available? proc) (error 'act! "widget action is unavailable" id action))
        (call-with-values (lambda () (apply (cdr proc) id (let ([f (event-frame)]) (if (and f (equal? id (frame-id f))) (frame-source f) source)) d arguments))
          (lambda result (head:wake-main!) (apply values result))))))

  (define (subscribe! mount tree)
    (let ([ids (fold-left
                 (lambda (out row)
                   (let ([source (if (cdr row) (view:source (cdr row)) (car row))])
                     (cond [(not source) out]
                           [(eq? (car source) 'buffer) (head:adopt-store-buffer! (cadr source)) out]
                           [(member source out) out] [else (cons source out)]))) '() tree)])
      (model:subscribe! ids
        (lambda (notice)
          (for-each (lambda (id) (let ([n (hashtable-ref nodes id #f)]) (when n (node-mirrored-set! n #f)))) (mount-ids mount))
          (head:wake-main!)))))
  (define (reconcile! mount tree)
    (let ([ids (map car tree)])
      (for-each (lambda (id)
                  (let ([n (hashtable-ref nodes id #f)])
                    (when (and n (eq? (node-root n) mount) (not (member id ids))) (hashtable-delete! nodes id)))) (mount-ids mount))
      (for-each
        (lambda (row)
          (let* ([id (car row)] [old (hashtable-ref nodes id #f)])
            (if (and old (eq? (node-root old) mount))
              (begin (node-mirrored-set! old #f) (node-key-set! old #f))
              (hashtable-set! nodes id (make-node id mount #f #f #f #f #f))))) tree)
      (mount-ids-set! mount ids)))

  (edoc "Attach a root tree to an opaque host slot. Repeating this attachment is idempotent; a second live host is refused."
        (id list "root view") (slot any "head-local host identity") (returns any))
  (define (mount! id slot)
    (let ([old (hashtable-ref roots id #f)])
      (cond [old (unless (eq? slot (mount-slot old)) (error 'mount! "view already has a host" id)) old]
        [else
         (kernel:call-with-runtime-registrations
           (lambda ()
             (let-values ([(status d) (interaction:claim! head:ui-actor id)])
               (unless (memq status '(applied unavailable)) (error 'mount! "view cannot be mounted" status id))
               (let* ([m (make-mount (datum:copy id) slot #f '())] [tree (rows id)])
                 (guard (ex [else
                             (when (mount-subscription m) (model:unsubscribe! (mount-subscription m)))
                             (when d (interaction:release! head:ui-actor id (view:generation d)))
                             (raise ex)])
                   (mount-subscription-set! m (subscribe! m tree))
                   (reconcile! m tree) (hashtable-set! roots id m) m)))))])))

  (edoc "Release a root and its recursive resources after publication; canonical views and sources survive."
        (id list "root id"))
  (define (unmount! id)
    (let ([m (hashtable-ref roots id #f)])
      (when m
        (cancel! id 'unmount)
        (let ([d (interaction:snapshot id)])
          (when d (interaction:release! head:ui-actor id (view:generation d))))
        (hashtable-delete! roots id)
        (hashtable-delete! preparations id)
        (for-each (lambda (id) (hashtable-delete! nodes id) (hashtable-delete! failures id)) (mount-ids m))
        (model:unsubscribe! (mount-subscription m)))))

  (edoc "Arrange owned trees, staging source demand before the guarded structural commit."
        (changes list "(parent revision children options) entries"))
  (define (arrange! changes)
    (kernel:call-with-runtime-registrations (lambda ()
                                              (let* ([affected (fold-left (lambda (out change)
                                                                            (let ([root (node-root (mounted (car change)))]) (if (memq root out) out (cons root out)))) '() changes)]
                                                     [extra (apply append (map (lambda (change)
                                                                                 (apply append (map (lambda (child) (if (interaction:snapshot (cadr child)) (rows (cadr child)) (view:tree (cadr child)))) (caddr change)))) changes))]
                                                     [staged '()])
                                                (for-each (lambda (change)
                                                            (for-each (lambda (child)
                                                                        (when (hashtable-contains? roots (cadr child))
                                                                          (error 'arrange! "unmount a root before nesting it" (cadr child)))) (caddr change))) changes)
                                                (dynamic-wind void
                                                  (lambda ()
                                                    (for-each (lambda (m) (set! staged (cons (cons m (subscribe! m (append (rows (mount-id m)) extra))) staged))) affected)
                                                    (let-values ([(status changed) (interaction:arrange! head:ui-actor changes
                                                                                     (map (lambda (m) (list (mount-id m) (view:generation (interaction:snapshot (mount-id m))))) affected))])
                                                      (when (eq? status 'applied)
                                                        (for-each (lambda (m) (cancel! (mount-id m) 'arrange)) affected)
                                                        (for-each (lambda (m)
                                                                    (let* ([tree (rows (mount-id m))] [token (subscribe! m tree)] [old (mount-subscription m)])
                                                                      (reconcile! m tree) (mount-subscription-set! m token) (model:unsubscribe! old))) affected)
                                                        (head:wake-main!))
                                                      (values status changed)))
                                                  (lambda () (for-each (lambda (p) (model:unsubscribe! (cdr p))) staged)))))))

  (define failures (make-hashtable equal-hash equal?))
  (define (option d name fallback)
    (cond [(and d (assq name (view:options d))) => cdr] [else fallback]))
  (define (projection! n entry source)
    (let ([key (list entry source)])
      (unless (equal? key (node-data-key n))
        (node-data-set! n ((field entry 'prepare values) source))
        (node-data-key-set! n key))
      (node-data n)))
  (define measurement-cache (make-parameter #f))
  (define (measure! id axis cross)
    (let* ([cache (measurement-cache)] [key (list id axis cross)] [old (and cache (hashtable-ref cache key #f))])
      (or old (let ([result (measure-node! id axis cross)])
                (when cache (hashtable-set! cache key result)) result))))
  (define (measure-node! id axis cross)
    (guard (ex [else '(1 1)])
      (let* ([n (mounted id)] [d (interaction:snapshot id)] [entry (definition d)])
        (let-values ([(available? source) (source! n d)])
          (let* ([proc (field entry 'measure #f)]
                 [result (if (and available? proc)
                           (proc (projection! n entry source) d axis cross measure!)
                           '(1 1))])
            (unless (and (list? result) (= (length result) 2)
                         (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0))) result)
                         (<= (car result) (cadr result))) (error 'measure! "invalid measurement" result))
            result)))))
  (define (locate! id anchor width)
    (let* ([n (mounted id)] [d (interaction:snapshot id)] [entry (definition d)])
      (let-values ([(available? source) (source! n d)])
        (if (and available? entry)
          (let ([proc (field entry 'locate #f)])
            (if proc (proc (projection! n entry source) anchor width) (locate-child! id anchor width)))
          0))))
  (define (linear-layout axis)
    (lambda (d width height measure locate)
      (let* ([children (view:children d)] [horizontal? (eq? axis 'x)]
             [gap (case (option d 'spacing 'none) [(normal) 1] [(wide) 2] [else 0])]
             [sizes (layout:linear (if horizontal? width height) gap
                      (map (lambda (child) (append (measure (cadr child) axis (if horizontal? height width)) (list (caddr child)))) children))])
        (map (lambda (child size)
               (list (cadr child) (if horizontal? (list (car size) 0 (cadr size) height) (list 0 (car size) width (cadr size))))) children sizes))))
  (define (overlay-layout d width height measure locate)
    (map (lambda (child) (list (cadr child) (list 0 0 width height))) (view:children d)))
  (define (scroll-layout d width height measure locate)
    (unless (= (length (view:children d)) 1) (error 'scroll "expected one child"))
    (let* ([id (cadar (view:children d))] [extent (max height (cadr (measure id 'y width)))]
           [at (min (max 0 (- extent height)) (max 0 (locate id (view:state d) width)))])
      (list (list id (list 0 (- at) width extent)))))
  (define (spacing d)
    (case (option d 'spacing 'none) [(normal) 1] [(wide) 2] [else 0]))
  (define (linear-measure direction)
    (lambda (data d axis cross measure)
      (let* ([children (view:children d)] [sizes (map (lambda (child) (measure (cadr child) axis cross)) children)]
             [along? (eq? direction axis)] [gap (if along? (* (spacing d) (max 0 (- (length sizes) 1))) 0)])
        (map (lambda (i) (+ gap (apply (if along? + max) (cons 0 (map (lambda (p) (list-ref p i)) sizes))))) '(0 1)))))
  (define (overlay-measure data d axis cross measure)
    (map (lambda (i) (apply max 0 (map (lambda (child) (list-ref (measure (cadr child) axis cross) i)) (view:children d)))) '(0 1)))
  (define (content-placements! id width)
    (let* ([d (interaction:snapshot id)] [entry (definition d)] [layout (field entry 'layout #f)])
      (if layout (layout d width (cadr (measure! id 'y width)) measure! locate!) '())))
  (define (anchor! id position width)
    (let* ([n (mounted id)] [d (interaction:snapshot id)] [entry (definition d)] [proc (field entry 'anchor #f)])
      (let-values ([(available? source) (source! n d)])
        (cond [(and available? proc) (proc (projection! n entry source) position width)]
          [else
           (let* ([placements (content-placements! id width)]
                  [p (or (find (lambda (p) (< position (+ (cadr (cadr p)) (cadddr (cadr p))))) placements)
                         (and (pair? placements) (car (reverse placements))))])
             (and p (let* ([ids (map car placements)] [tail (member (car p) ids)])
                      (list 'child (car p) (anchor! (car p) (max 0 (- position (cadr (cadr p)))) (caddr (cadr p)))
                        (cdr tail) (reverse (list-head ids (- (length ids) (length tail))))))))]))))
  (define (locate-child! id anchor width)
    (let* ([placements (content-placements! id width)]
           [find-id (lambda (id) (find (lambda (p) (equal? id (car p))) placements))]
           [p (and (list? anchor) (= (length anchor) 5) (eq? (car anchor) 'child)
                (or (find-id (cadr anchor))
                  (exists find-id (append (list-ref anchor 3) (list-ref anchor 4)))))])
      (if p (+ (cadr (cadr p)) (if (equal? (car p) (cadr anchor)) (locate! (car p) (caddr anchor) (caddr (cadr p))) 0)) 0)))
  (define (find-frame frame id)
    (and frame (if (equal? id (frame-id frame)) frame (exists (lambda (f) (find-frame f id)) (frame-children frame)))))
  (define (allocation id)
    (or (exists (lambda (p) (find-frame (car p) id)) presentations)
      (find-frame (prepared (mount-id (node-root (mounted id)))) id)))
  (define (scroll-action! id source d delta)
    (let* ([frame (allocation id)] [child (and (= 1 (length (view:children d))) (cadar (view:children d)))])
      (unless (and frame child (integer? delta) (exact? delta)) (error 'scroll "expected an allocated viewport and integer delta"))
      (let* ([width (caddr (frame-rect frame))] [height (cadddr (frame-rect frame))]
             [limit (max 0 (- (cadr (measure! child 'y width)) height))]
             [old (min limit (max 0 (locate! child (view:state d) width)))]
             [next (min limit (max 0 (+ old delta)))])
        (interaction:set-state! head:ui-actor id #f (anchor! child next width))
        (- delta (- next old)))))

  (define (rectangle? r)
    (and (list? r) (= (length r) 4) (for-all (lambda (n) (and (integer? n) (exact? n))) r)
      (>= (caddr r) 0) (>= (cadddr r) 0)))
  (define (composite clip lines children)
    (let* ([width (caddr clip)] [height (cadddr clip)]
           [canvas (list->vector (append lines (make-list (max 0 (- height (length lines))) (make-string width #\space))))])
      (for-each
        (lambda (child)
          (let* ([area (frame-clip child)] [x (- (car area) (car clip))] [y (- (cadr area) (cadr clip))])
            (for-each
              (lambda (line row)
                (let ([old (vector-ref canvas (+ row y))])
                  (vector-set! canvas (+ row y)
                    (string-append (glyph:slice old 0 x) line
                      (glyph:slice old (+ x (caddr area)) (- width x (caddr area)))))))
              (frame-lines child) (iota (length (frame-lines child)))))) children)
      (vector->list canvas)))
  (define (build-frame! id rect parent-clip)
    (let* ([n (mounted id)] [d (interaction:snapshot id)] [entry (definition d)] [clip (layout:intersect rect parent-clip)])
      (let-values ([(available? source) (source! n d)])
        (define (placeholder text)
          (make-frame id d #f source rect clip '()
            (if (or (zero? (caddr clip)) (zero? (cadddr clip))) '() (list (glyph:fit text (caddr clip))))))
        (guard (ex [else
                    (let ([basis (list entry source)])
                      (unless (equal? basis (hashtable-ref failures id #f))
                        (hashtable-set! failures id basis) (echo:set-text! (kernel:condition-text ex))))
                    (placeholder (format "[Widget failed ~s]" id))])
          (cond
            [(or (zero? (caddr clip)) (zero? (cadddr clip))) (make-frame id d entry source rect clip '() '())]
            [(not (and entry available?)) (placeholder (format "[Unavailable widget ~s]" id))]
            [else
             (let* ([data (projection! n entry source)] [width (caddr rect)] [height (cadddr rect)]
                    [range (cons (- (cadr clip) (cadr rect)) (cadddr clip))]
                    [key (list entry source (view:state d) width height range (- (car clip) (car rect)) (caddr clip))]
                    [render (field entry 'render #f)] [layout (field entry 'layout #f)]
                    [placements (if layout (layout d width height measure! locate!) '())])
               (unless (and (list? placements)
                         (let check ([rest placements] [seen '()])
                           (or (null? rest) (let ([p (car rest)])
                                              (and (list? p) (= (length p) 2) (member (car p) (map cadr (view:children d)))
                                                (not (member (car p) seen)) (rectangle? (cadr p)) (check (cdr rest) (cons (car p) seen)))))))
                 (error 'prepare! "invalid child placements" id placements))
               (unless (equal? key (node-key n))
                 (let ([lines (if render (render data (view:state d) width height range) '())])
                   (unless (and (list? lines) (for-all string? lines)) (error 'prepare! "expected display lines"))
                   (node-lines-set! n
                     (map (lambda (line) (glyph:slice line (- (car clip) (car rect)) (caddr clip)))
                       (list-head lines (min (cdr range) (length lines)))))
                   (node-key-set! n key)))
               (let ([children (map (lambda (p) (build-frame! (car p) (layout:translate (cadr p) (car rect) (cadr rect)) clip)) placements)])
                 (make-frame id d entry source rect clip children
                   (if (and (null? children) (option d 'pass-through #f)) (node-lines n) (composite clip (node-lines n) children)))))])))))

  (edoc "Prepare a recursive frame for a root allocation. Geometry and borrowed source snapshots stay in the head; preparation does not make hits live."
        (id list "root view") (width integer "nonnegative backend width") (height integer "nonnegative backend height") (returns any))
  (define (prepare! id width height)
    (let ([rect (list 0 0 width height)])
      (unless (rectangle? rect) (error 'prepare! "invalid allocation" rect))
      (parameterize ([measurement-cache (make-hashtable equal-hash equal?)])
        (let ([frame (build-frame! id rect rect)]) (hashtable-set! preparations id frame) frame))))

  (edoc "Read the latest prepared frame, which may not have been displayed."
        (id list "root view") (returns any))
  (define (prepared id) (hashtable-ref preparations id #f))

  (edoc "Adopt the exact frames whose output was successfully flushed. The painter calls this before publication hooks."
        (placements list "(frame screen-x screen-y) entries"))
  (define (present! placements) (set! presentations placements))

  (edoc "Read shown placements; input must use these instead of prepared geometry." (returns list))
  (define (shown) presentations)

  (edoc "Invalidate uncertain output. Geometry-dependent input stays disabled until a successful full presentation.")
  (define (invalidate!) (defer-cancel! 'output-failure) (set! hover-target #f) (set! presentations '()))

  ;; Routing targets and event frames are dynamic head context, never wire data.
  (edoc "The explicit receiver of the current widget key binding or event, or #f." (returns any))
  (define target (make-parameter #f))

  (edoc "The shown frame supplying the current pointer event's source basis, or #f." (returns any))
  (define event-frame (make-parameter #f))
  (define pointer-capture #f)
  (define hover-target #f)
  (define cancelled-gestures '())
  (define (defer-cancel! reason)
    (when pointer-capture (set! cancelled-gestures (cons (cons pointer-capture reason) cancelled-gestures)))
    (set! pointer-capture #f))
  (define (drain-cancels!)
    (let ([pending (reverse cancelled-gestures)])
      (set! cancelled-gestures '())
      (for-each (lambda (p)
                  (let* ([f (car p)] [entry (frame-definition f)] [handler (field entry 'event #f)])
                    (when (and handler (eq? entry (definition (frame-descriptor f))))
                      (parameterize ([target (frame-id f)] [event-frame f])
                        (handler (frame-id f) (frame-source f) (frame-descriptor f) (list 'cancel (cdr p))))))) pending)))
  (define last-focus (make-hashtable equal-hash equal?))
  (define (live-frame? f)
    (let* ([n (hashtable-ref nodes (frame-id f) #f)] [d (and n (interaction:snapshot (frame-id f)))])
      (and d (frame-descriptor f) (eq? (frame-definition f) (definition d))
        (= (view:generation d) (view:generation (frame-descriptor f))))))
  (define (shown-root id)
    (exists (lambda (p) (and (equal? id (frame-id (car p))) (car p))) presentations))
  (define (visible? f)
    (and (> (caddr (frame-clip f)) 0) (> (cadddr (frame-clip f)) 0) (live-frame? f)))
  (define (modal f)
    (and (visible? f) (or (exists modal (reverse (frame-children f)))
                        (and (option (frame-descriptor f) 'modal #f) f))))
  (define (focus-order f)
    (if (or (zero? (caddr (frame-clip f))) (zero? (cadddr (frame-clip f)))) '()
      (append (if (field (frame-definition f) 'focus #f) (list (frame-id f)) '())
        (apply append (map focus-order (frame-children f))))))
  (define (focusable f)
    (filter (lambda (id) (live-frame? (find-frame f id))) (focus-order f)))
  (define (path id)
    (let loop ([id id] [out '()])
      (let ([d (and id (interaction:snapshot id))])
        (if d (loop (view:parent d) (cons id out)) out))))
  (define (send! id event frame)
    (let* ([n (hashtable-ref nodes id #f)] [d (and n (interaction:snapshot id))] [entry (definition d)]
           [handler (field entry 'event #f)])
      (and handler (or (not frame) (live-frame? frame))
        (let-values ([(available? source) (source! n d)])
          (and available?
            (parameterize ([target id] [event-frame frame])
              (and (handler id (if frame (frame-source frame) source) d event) #t)))))))
  (define (focus-frame root)
    (let ([f (if (event-frame) (shown-root root) (or (prepared root) (shown-root root)))]) (and f (or (modal f) f))))

  (edoc "Focus a visible accepting descendant within the root's current modal scope. Hidden roots retain their remembered target."
        (root list "root view") (id list "descendant view"))
  (define (focus! root id)
    (let* ([frame (focus-frame root)] [d (interaction:snapshot root)] [old (and d (view:focus d))])
      (unless (and frame d (member id (focusable frame))) (error 'focus! "target is not focusable here" root id))
      (unless (equal? old id)
        (let common ([before (path old)] [after (path id)])
          (if (and (pair? before) (pair? after) (equal? (car before) (car after))) (common (cdr before) (cdr after))
            (begin
              (for-each (lambda (id) (send! id '(blur) #f)) (reverse before))
              (interaction:focus! root id)
              (for-each (lambda (id) (send! id '(focus) #f)) after))))
        (hashtable-set! last-focus root id) (head:wake-main!))))
  (define (ensure-focus! root)
    (let* ([frame (focus-frame root)] [choices (if frame (focusable frame) '())]
           [d (interaction:snapshot root)] [old (and d (view:focus d))]
           [previous (or old (hashtable-ref last-focus root #f))]
           [past (let ([old-frame (shown-root root)]) (if old-frame (focus-order old-frame) '()))]
           [tail (and previous (member previous past))]
           [next (or (and (member old choices) old)
                   (and tail (find (lambda (id) (member id choices)) (cdr tail)))
                   (and tail (find (lambda (id) (member id choices)) (reverse (list-head past (- (length past) (length tail))))))
                   (and (pair? choices) (car choices)))])
      (cond [next (focus! root next)] [(and d old) (interaction:focus! root #f)]) next))

  (edoc "Build ordered keymap receivers and a chord ownership basis for this root. The outer host may append its own contexts."
        (root list "active root") (key string "first key token") (returns list "(basis scopes focused-view)"))
  (define (key-scopes! root key)
    (drain-cancels!)
    (let* ([focus (ensure-focus! root)] [scope (focus-frame root)]
           [path (if focus (path focus) (if scope (path (frame-id scope)) (list root)))] [barrier (and scope (option (frame-descriptor scope) 'modal #f) (frame-id scope))]
           [normal (let loop ([rest (reverse path)] [out '()])
                     (if (null? rest) (reverse out)
                       (let* ([id (car rest)] [d (interaction:snapshot id)] [entry (definition d)]
                              [full? (eq? (field entry 'capture 'partial) 'full)]
                              [yield? (and (not full?) (member key (field entry 'yield '())))]
                              [item (list id (if yield? '() (field entry 'contexts '())) (or full? (equal? id barrier)))])
                         (if (equal? id barrier) (reverse (cons item out)) (loop (cdr rest) (cons item out))))))]
           [captures (filter (lambda (scope) (pair? (cadr scope)))
                       (map (lambda (id) (list id (field (definition (interaction:snapshot id)) 'capture-contexts '()) #f)) path))]
           [basis (map (lambda (id) (let ([d (interaction:snapshot id)]) (list id (and d (view:generation d)) (definition d)))) path)])
      (list (list root focus barrier basis) (append captures normal) focus)))

  (edoc "Offer committed text or an unbound normalized key to the focused path; a full capture or modal boundary stops bubbling."
        (root list "active root") (event list "(text string typed-or-paste), (key token), or cancellation") (returns boolean))
  (define (input! root event)
    (drain-cancels!)
    (let* ([focus (ensure-focus! root)] [scope (focus-frame root)]
           [barrier (and scope (option (frame-descriptor scope) 'modal #f) (frame-id scope))])
      (let loop ([ids (reverse (if focus (path focus) (if scope (path (frame-id scope)) (list root))))])
        (and (pair? ids)
          (let* ([id (car ids)] [d (interaction:snapshot id)] [entry (definition d)])
            (or (and (not (and (eq? (car event) 'key) (member (cadr event) (field entry 'yield '()))))
                     (or (send! id (if (eq? (car event) 'key) (list-head event 2) event) #f)
                       (and (eq? (car event) 'key) (= (length event) 3) (caddr event)
                         (send! id (list 'text (caddr event) 'typed) #f))))
                (eq? (field entry 'capture 'partial) 'full) (equal? id barrier) (loop (cdr ids))))))))

  (edoc "Capture pointer motion and release for the current shown press target, including outside its allocation."
        (id list "current event receiver"))
  (define (capture! id)
    (let ([f (event-frame)])
      (unless (and f (equal? id (frame-id f)) (live-frame? f)) (error 'capture! "capture requires a live shown target" id))
      (set! pointer-capture f)))

  (edoc "Cancel a root's pointer gesture when its host hides, blurs or loses the device; retain logical focus."
        (root list "root") (reason symbol "cancellation reason"))
  (define (cancel! root reason)
    (let ([n (and pointer-capture (hashtable-ref nodes (frame-id pointer-capture) #f))])
      (when (and n (equal? root (mount-id (node-root n)))) (defer-cancel! reason)))
    (drain-cancels!))
  (define (hit f x y)
    (and (layout:contains? (frame-clip f) x y)
      (or (exists (lambda (child) (hit child x y)) (reverse (frame-children f)))
        (and (or (option (frame-descriptor f) 'modal #f) (not (option (frame-descriptor f) 'pass-through #f))) f))))

  (edoc "Route normalized pointer/scroll input through shown frames. Return (root focus-host?) when consumed, or #f outside widgets."
        (event list "(pointer phase button modifiers) or (scroll dx dy units)")
        (x integer "screen x, zero based") (y integer "screen y, zero based") (returns any))
  (define (pointer! event x y)
    (drain-cancels!)
    (let* ([captured (and pointer-capture (live-frame? pointer-capture) (not (eq? (car event) 'scroll)))]
           [placement (if captured
                        (find (lambda (p) (find-frame (car p) (frame-id pointer-capture))) presentations)
                        (find (lambda (p) (layout:contains? (frame-clip (car p)) (- x (cadr p)) (- y (caddr p)))) (reverse presentations)))]
           [root (and placement (car placement))]
           [x (if placement (- x (cadr placement)) x)] [y (if placement (- y (caddr placement)) y)]
           [f (and root (if captured (find-frame root (frame-id pointer-capture)) (hit (or (modal root) root) x y)))])
      (unless captured (when pointer-capture (defer-cancel! 'stale-target)))
      (when (and (eq? (car event) 'pointer) (eq? (cadr event) 'move))
        (unless (and hover-target f (equal? (frame-id hover-target) (frame-id f)))
          (when hover-target (send! (frame-id hover-target) '(pointer leave none () 0 0) hover-target))
          (set! hover-target f)))
      (and root
        (begin
          (when (and f (live-frame? f))
            (if (eq? (car event) 'scroll)
              (let loop ([ids (reverse (path (frame-id f)))] [left (caddr event)])
                (unless (or (null? ids) (zero? left))
                  (let* ([id (car ids)] [d (interaction:snapshot id)] [entry (definition d)]
                         [scroll? (assq 'scroll (field entry 'actions '()))])
                    (loop (cdr ids) (if scroll? (act! id 'scroll left) left)))))
              (begin
                (when (and (eq? (cadr event) 'press) (field (frame-definition f) 'focus #f)) (parameterize ([event-frame f]) (focus! (frame-id root) (frame-id f))))
                (send! (frame-id f) (append event (list (- x (car (frame-rect f))) (- y (cadr (frame-rect f))))) f))))
          (when (and (eq? (car event) 'pointer) (memq (cadr event) '(release leave))) (set! pointer-capture #f))
          (list (frame-id root) (and f (eq? (car event) 'pointer) (eq? (cadr event) 'press)
                                     (field (frame-definition f) 'focus #f)))))))

  ;; The minimal text widget deliberately consumes arbitrary model values.
  ;; It needs no extra base dataset service or evaluator allocation.
  (define (text-data model)
    (let ([value (cdr (assq 'value model))])
      (cons (cdr (assq 'revision model)) (list->vector (string:lines (if (string? value) value (format "~s" value)))))))
  (define (text-source! id source descriptor)
    (cdr (projection! (mounted id) (definition descriptor) source)))
  (define (text-state state count)
    (if (and (integer? state) (exact? state)) (max 0 (min (- count 1) state)) 0))
  (define (text-render data state width height range)
    (let* ([data (cdr data)] [count (vector-length data)] [selected (text-state state count)] [top (min count (car range))])
      (map (lambda (i) (let ([row (+ top i)])
                         (string-append (if (= row selected) "> " "  ") (vector-ref data row)))) (iota (min (cdr range) (- count top))))))
  (define (text-move! id source d offset)
    (let* ([data (text-source! id source d)] [count (vector-length data)] [row (text-state (+ (text-state (view:state d) count) offset) count)])
      (interaction:set-state! head:ui-actor id (cdr (assq 'revision source)) row)
      (reveal! id (list (cdr (assq 'revision source)) row))))
  (define (text-select! id source d row)
    (interaction:set-state! head:ui-actor id (cdr (assq 'revision source)) (text-state row (vector-length (text-source! id source d)))))
  (define (text-choose! id source d)
    (let* ([data (text-source! id source d)] [row (text-state (view:state d) (vector-length data))] [text (vector-ref data row)])
      (echo:set-text! text) (list (view:source d) (cdr (assq 'revision source)) row text)))
  (define (text-event! id source d event)
    (and (eq? (car event) 'pointer) (eq? (cadr event) 'press) (eq? (caddr event) 'primary)
      (begin (act! id 'select (list-ref event 5)) #t)))

  (edoc "Reveal a logical source anchor in its nearest containing scroll viewport, without changing selection."
        (id list "descendant") (anchor datum "source anchor"))
  (define (reveal! id anchor)
    (let loop ([child id] [rest (cdr (reverse (path id)))] [anchor anchor])
      (unless (null? rest)
        (let* ([parent (car rest)] [d (interaction:snapshot parent)] [f (allocation parent)])
          (if (and f (eq? (view:kind d) 'scroll))
            (let* ([width (caddr (frame-rect f))] [height (cadddr (frame-rect f))]
                   [point (locate! child anchor width)] [top (locate! child (view:state d) width)]
                   [delta (cond [(< point top) (- point top)] [(>= point (+ top height)) (+ 1 (- point top height))] [else 0])])
              (unless (zero? delta) (act! parent 'scroll delta)))
            (loop parent (cdr rest) (list 'child child anchor '() '())))))))

  (edoc "Install the text definition and renderer invalidation.")
  (define (init!)
    (register! 'text 2 (list (cons 'prepare text-data) (cons 'render text-render)
                         (cons 'measure (lambda (data descriptor axis cross measure) (if (eq? axis 'y) (list 1 (vector-length (cdr data))) '(1 1))))
                         (cons 'locate (lambda (data anchor width)
                                         (if (and (list? anchor) (= (length anchor) 2) (equal? (car anchor) (car data)) (integer? (cadr anchor))) (max 0 (cadr anchor)) 0)))
                         (cons 'anchor (lambda (data position width) (list (car data) position))) (cons 'focus #t)
                         (cons 'contexts '(widget-text)) (cons 'event text-event!)
                         (cons 'actions (list (cons 'move text-move!) (cons 'select text-select!) (cons 'choose text-choose!)))))
    (keymap:bind-default! 'widget-text "UP" (keymap:call act! target 'move -1))
    (keymap:bind-default! 'widget-text "DOWN" (keymap:call act! target 'move 1))
    (keymap:bind-default! 'widget-text "RET" (keymap:call act! target 'choose))
    (register! 'row 1 (list (cons 'layout (linear-layout 'x)) (cons 'measure (linear-measure 'x))))
    (register! 'column 1 (list (cons 'layout (linear-layout 'y)) (cons 'measure (linear-measure 'y))))
    (register! 'overlay 1 (list (cons 'layout overlay-layout) (cons 'measure overlay-measure)))
    (register! 'scroll 1 (list (cons 'layout scroll-layout) (cons 'measure overlay-measure) (cons 'actions (list (cons 'scroll scroll-action!)))))
    (head:add-pre-redraw-hook! drain-cancels!)
    (kernel:registry-observe! definitions
      (lambda (removed added)
        (when (and pointer-capture (not (live-frame? pointer-capture))) (defer-cancel! 'reload))
        (head:wake-main!))))
)
