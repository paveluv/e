;; Recursive mounts are head runtime objects, independent of window buffers.
(import (only (foundation edoc) elibrary))
(elibrary (head widget)
  (export act! actions arrange! frame-children frame-clip frame-descriptor frame-id frame-lines frame-rect frame-source init! invalidate! mount! prepare! prepared present! register! shown unmount!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:)
          (prefix (foundation string) string:)
          (prefix (head echo) echo:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
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
                        [(contexts) (and (list? (cdr p)) (for-all symbol? (cdr p)))]
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
        (call-with-values (lambda () (apply (cdr proc) id source d arguments))
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
  (define (invalidate!) (set! presentations '()))

  ;; The minimal text widget deliberately consumes arbitrary model values.
  ;; It needs no extra base dataset service or evaluator allocation.
  (define (text-data model)
    (let ([value (cdr (assq 'value model))])
      (cons (cdr (assq 'revision model)) (list->vector (string:lines (if (string? value) value (format "~s" value)))))))
  (define (text-source! id source descriptor)
    (cdr (projection! (mounted id) (definition descriptor) source)))
  (define (text-state state count)
    (map (lambda (n) (if (and (integer? n) (exact? n)) (max 0 (min (- count 1) n)) 0))
      (if (and (list? state) (= (length state) 2)) state '(0 0))))
  (define (text-render data state width height range)
    (let* ([data (cdr data)] [count (vector-length data)] [state (text-state state count)]
           [top (min count (+ (cadr state) (car range)))])
      (map (lambda (i) (let ([row (+ top i)])
                         (string-append (if (= row (car state)) "> " "  ") (vector-ref data row))))
        (iota (min (cdr range) (- count top))))))
  (define (text-move! id model descriptor offset height)
    (let* ([count (vector-length (text-source! id model descriptor))] [state (text-state (view:state descriptor) count)]
           [row (max 0 (min (- count 1) (+ (car state) offset)))]
           [top (min row (max (cadr state) (- row (max 1 height) -1)))])
      (interaction:set-state! head:ui-actor id (cdr (assq 'revision model)) (list row top)) (void)))
  (define (text-scroll! id model descriptor offset)
    (let* ([count (vector-length (text-source! id model descriptor))] [state (text-state (view:state descriptor) count)])
      (interaction:set-state! head:ui-actor id (cdr (assq 'revision model))
        (list (car state) (max 0 (min (- count 1) (+ (cadr state) offset))))) (void)))
  (define (text-select! id model descriptor row)
    (let* ([count (vector-length (text-source! id model descriptor))] [state (text-state (view:state descriptor) count)])
      (interaction:set-state! head:ui-actor id (cdr (assq 'revision model))
        (list (max 0 (min (- count 1) (+ (cadr state) row))) (cadr state)))) (void))
  (define (text-choose! id model descriptor)
    (let* ([lines (text-source! id model descriptor)] [row (car (text-state (view:state descriptor) (vector-length lines)))]
           [chosen (vector-ref lines row)])
      (echo:set-text! chosen)
      (list (view:source descriptor) (cdr (assq 'revision model)) row chosen)))
  (define (text-input! id model descriptor event point size)
    (cond [(member event '("UP" "DOWN")) (act! id 'move (if (string=? event "UP") -1 1) (cadr size)) #t]
          [(member event '("WHEEL-UP" "WHEEL-DOWN")) (act! id 'scroll (if (string=? event "WHEEL-UP") -3 3)) #t]
          [(string=? event "MOUSE-CLICK") (when point (act! id 'select (car point))) 'keep-focus]
          [(string=? event "RET") (act! id 'choose) #t]
          [else #f]))


  (edoc "Install the text definition and renderer invalidation.")
  (define (init!)
    (register! 'text 1 (list (cons 'prepare text-data) (cons 'render text-render)
                         (cons 'measure (lambda (data descriptor axis cross measure) (if (eq? axis 'y) (list 1 (vector-length (cdr data))) '(1 1))))
                         (cons 'locate (lambda (data anchor width)
                                         (if (and (list? anchor) (= (length anchor) 2) (equal? (car anchor) (car data)) (integer? (cadr anchor))) (max 0 (cadr anchor)) 0)))
                         (cons 'anchor (lambda (data position width) (list (car data) position))) (cons 'focus #t)
                         (cons 'actions (list (cons 'input text-input!) (cons 'move text-move!) (cons 'scroll text-scroll!) (cons 'select text-select!) (cons 'choose text-choose!)))))
    (register! 'row 1 (list (cons 'layout (linear-layout 'x)) (cons 'measure (linear-measure 'x))))
    (register! 'column 1 (list (cons 'layout (linear-layout 'y)) (cons 'measure (linear-measure 'y))))
    (register! 'overlay 1 (list (cons 'layout overlay-layout) (cons 'measure overlay-measure)))
    (register! 'scroll 1 (list (cons 'layout scroll-layout) (cons 'measure overlay-measure) (cons 'actions (list (cons 'scroll scroll-action!)))))
    (kernel:registry-observe! definitions (lambda (removed added) (head:wake-main!))))
)
