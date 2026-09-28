;; Recursive mounts are head runtime objects, independent of window buffers.
(import (only (foundation edoc) elibrary))
(elibrary (head widget)
  (export act! actions arrange! init! mount! register! render! unmount!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:)
          (prefix (foundation string) string:)
          (prefix (head echo) echo:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (state model) model:)
          (prefix (state view) view:)
          (prefix (sys glyph) glyph:))

  (define definitions (kernel:make-registry car))
  (define roots (make-hashtable equal-hash equal?))
  (define nodes (make-hashtable equal-hash equal?))
  (define-record-type mount (fields id slot (mutable subscription) (mutable ids)))
  (define-record-type node (fields id root (mutable mirrored) (mutable key) (mutable lines)))
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
                        [(render measure layout event) (procedure? (cdr p))]
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
        (call-with-values (lambda () (apply (cdr proc) id (datum:copy source) d arguments))
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
              (hashtable-set! nodes id (make-node id mount #f #f #f))))) tree)
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
        (for-each (lambda (id) (hashtable-delete! nodes id)) (mount-ids m))
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

  (edoc "Prepare clipped lines for an allocation. Source reads precede callbacks; warm allocations reuse output."
        (id list "view") (width integer "nonnegative width") (height integer "nonnegative height") (returns list))
  (define (render! id width height)
    (unless (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0))) (list width height)) (error 'render! "invalid allocation"))
    (let* ([n (mounted id)] [d (interaction:snapshot id)] [entry (definition d)])
      (let-values ([(available? source) (source! n d)])
        (let ([key (list entry available? source d width height)])
          (unless (equal? key (node-key n))
            (let* ([render (field entry 'render #f)]
                   [lines (if (or (zero? width) (zero? height)) '()
                            (if (and entry available?)
                              (if render (render (datum:copy source) (and d (datum:copy (view:state d))) width height) '())
                              (list (format "[Unavailable widget ~s]" id))))])
              (unless (and (list? lines) (for-all string? lines)) (error 'render! "expected display lines"))
              (node-lines-set! n (map (lambda (line) (glyph:fit line width)) (list-head lines (min height (length lines)))))
              (node-key-set! n key)))
          (node-lines n)))))
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
    (let* ([count (length (text-lines model))] [state (text-state (view:state descriptor) count)]
           [row (max 0 (min (- count 1) (+ (car state) offset)))]
           [top (min row (max (cadr state) (- row (max 1 height) -1)))])
      (interaction:set-state! head:ui-actor id (cdr (assq 'revision model)) (list row top)) (void)))
  (define (text-scroll! id model descriptor offset)
    (let* ([count (length (text-lines model))] [state (text-state (view:state descriptor) count)])
      (interaction:set-state! head:ui-actor id (cdr (assq 'revision model))
        (list (car state) (max 0 (min (- count 1) (+ (cadr state) offset))))) (void)))
  (define (text-select! id model descriptor row)
    (let* ([count (length (text-lines model))] [state (text-state (view:state descriptor) count)])
      (interaction:set-state! head:ui-actor id (cdr (assq 'revision model))
        (list (max 0 (min (- count 1) (+ (cadr state) row))) (cadr state)))) (void))
  (define (text-choose! id model descriptor)
    (let* ([lines (text-lines model)] [row (car (text-state (view:state descriptor) (length lines)))]
           [chosen (list-ref lines row)])
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
    (register! 'text 1 (list (cons 'render text-render) (cons 'focus #t)
                         (cons 'actions (list (cons 'input text-input!) (cons 'move text-move!) (cons 'scroll text-scroll!) (cons 'select text-select!) (cons 'choose text-choose!)))))
    (kernel:registry-observe! definitions (lambda (removed added) (head:wake-main!))))
)
