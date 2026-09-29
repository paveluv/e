;; Editor widget implementation. Public commands are re-exported by edit.
(import (only (foundation edoc) elibrary))
(elibrary (head editor)
  (export create-view! delete! expression! history! insert! move! page! paste! register! scroll! select! set-mark! transfer!)
  (import (chezscheme) (prefix (core descriptor) descriptor:) (prefix (core kernel) kernel:)
          (prefix (foundation string) string:) (prefix (foundation text) text:) (prefix (head expression) expression:)
          (prefix (head head) head:) (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head mode) mode:) (prefix (head render) render:)
          (prefix (head text-control) text-control:) (prefix (head text-layout) text-layout:)
          (prefix (head text-source) text-source:) (prefix (head widget) widget:)
          (prefix (service document) document:) (prefix (state store) store:)
          (prefix (state view) view:) (prefix (sys glyph) glyph:) (prefix (sys tty) tty:))

  (define (refuse message) (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (option d key fallback) (cond [(assq key (view:options d)) => cdr] [else fallback]))
  (define (position? p)
    (and (pair? p) (integer? (car p)) (exact? (car p)) (>= (car p) 0)
      (integer? (cdr p)) (exact? (cdr p)) (>= (cdr p) 0)))
  (define (state d)
    (let ([s (view:state d)])
      (unless (and (list? s) (= (length s) 4) (for-all position? (list-head s 3)) (boolean? (cadddr s)))
        (error 'editor "expected (caret anchor top marked?)" s)) s))
  (define (points source d)
    (text-source:rebase (list-head (state d) 3)
      (text-source:changes (text-control:mirror source) (or (view:basis d) (text-control:revision source)) (text-control:revision source))))
  (define-record-type mount (fields (mutable mode) (mutable facts) (mutable dimensions) (mutable goal) (mutable group)))
  (define mounts (make-hashtable equal-hash equal?))
  (define (mounted id)
    (or (hashtable-ref mounts id #f)
      (let ([m (make-mount #f '() #f #f #f)]) (hashtable-set! mounts id m) m)))
  (define dragging #f)
  (define (release! id)
    (hashtable-delete! mounts id)
    (when (equal? dragging id) (set! dragging #f)))

  (edoc "Create an unmounted editor view over a shared document. Caret, anchor, logical top and mark activity belong to the view. Options currently include wrap (boolean); no window is created."
        (actor actor "creator") (document integer "store document identity") (options list "logical preferences") (returns model))
  (define (create-view! actor document options)
    (unless (and (integer? document) (exact? document) (> document 0)
              (list? options) (for-all (lambda (p) (and (pair? p) (eq? (car p) 'wrap) (boolean? (cdr p)))) options)
              (<= (length options) 1)) (error 'create-view! "invalid document or editor options"))
    (view:create! actor (list 'buffer document) 'editor 1 options '((0 . 0) (0 . 0) (0 . 0) #f)))

  (define (service! id frame)
    (let* ([d (interaction:snapshot id)] [ref (and d (view:source d))])
      (when (and ref (eq? (car ref) 'buffer) (text-source:lookup (cadr ref))
              (store:visible? head:ui-actor (cadr ref)))
        (let-values ([(source d) (text-control:context id 'editor)])
          (let* ([m (mounted id)] [document (text-source:id (text-control:mirror source))]
                 [name (store:property document 'mode #f)] [mode (and name (mode:find name))]
                 [facts (map (lambda (key) (cons key (store:property document key #f))) (mode:required-facts mode))]
                 [signature (list mode (and mode (mode:render mode)) (and mode (mode:row-styles mode)) (and mode (mode:styles mode)))])
            (unless (equal? (widget:focused id) id) (mount-group-set! m #f))
            ;; Metadata acquisition is on the service path, never paint or motion.
            (unless (and (equal? signature (mount-mode m)) (equal? facts (mount-facts m)))
              (mount-mode-set! m signature) (mount-facts-set! m facts) (widget:repaint! id #t)))))))
  (define (prepare id source inputs)
    (text-control:mirror source)
    (let* ([m (mounted id)] [lines (text-control:lines source)]
           [frame (render:prepare #f #f lines (text-control:revision source) '())])
      (list id source frame (mode:source lines (mount-facts m)) (and (mount-mode m) (car (mount-mode m))))))
  (define (snap lines frame p)
    (let* ([row (min (car p) (- (vector-length lines) 1))]
           [col (min (cdr p) (string-length (vector-ref lines row)))])
      (cons row (render:character frame row (render:column frame row col)))))
  (define (address lines width p)
    (cons (car p) (if width (text-layout:segment (text-layout:breaks (vector-ref lines (car p)) width) (cdr p)) 0)))
  ;; A projection is (data points top left width height rows). Wrapped segment
  ;; addresses, horizontal cells and desired columns never enter the view.
  (define (project data d width height reveal?)
    (let* ([source (cadr data)] [lines (text-control:lines source)] [frame (caddr data)]
           [ps (points source d)] [wrap (and (option d 'wrap #t) (max 1 width))]
           [ps (and ps (map (lambda (p) (snap lines frame p)) ps))])
      (if (not ps) (list data #f '(0 . 0) 0 width height)
        (let-values ([(caret top left) (text-layout:scroll lines frame wrap width height 0
                                         (address lines wrap (caddr ps)) 0 (car ps) 0)])
          (list data ps (if reveal? top (address lines wrap (caddr ps))) left width height)))))
  (define (viewport data d width height range)
    (let* ([m (mounted (car data))] [dimensions (cons width height)]
           [g (project data d width height #f)] [source (cadr data)] [lines (text-control:lines source)]
           [top (caddr g)] [left (cadddr g)] [wrap (and (option d 'wrap #t) (max 1 width))]
           [frame (render:prepare (caddr data) #f lines (text-control:revision source)
                    (list (cons (car top) (+ (car top) height))))]
           [data (cons* (car data) source frame (cdddr data))] [cache (make-eqv-hashtable)])
      (unless (equal? dimensions (mount-dimensions m)) (mount-goal-set! m #f))
      (mount-dimensions-set! m dimensions)
      (cons data (append (cdr g) (list (if (not (cadr g)) '()
                                         (let loop ([y 0] [row (car top)] [segment (cdr top)] [out '()])
                                           (if (or (>= y (+ (car range) (cdr range))) (>= row (vector-length lines))) (reverse out)
                                             (let* ([breaks (and wrap (text-layout:breaks (vector-ref lines row) wrap))]
                                                    [next? (and breaks (< (+ segment 1) (vector-length breaks)))]
                                                    [start (if breaks (render:column frame row (vector-ref breaks segment)) left)]
                                                    [end (if next? (render:column frame row (vector-ref breaks (+ segment 1)))
                                                           (render:column frame row (string-length (vector-ref lines row))))])
                                               (loop (+ y 1) (if next? row (+ row 1)) (if next? (+ segment 1) 0)
                                                 (if (< y (car range)) out
                                                   (cons (append (list y row start end)
                                                           (or (hashtable-ref cache row #f)
                                                             (let ([value (call-with-values (lambda () (row-presentation data row)) list)])
                                                               (hashtable-set! cache row value) value))) out))))))))))))
  (define (row-presentation data row)
    (let* ([source (list-ref data 3)] [mode (list-ref data 4)] [line (vector-ref (mode:source-lines source) row)]
           [replacement (and mode (mode:render mode))] [styler (and mode (mode:row-styles mode))])
      (render:present (caddr data) row line
        (and replacement (guard (ex [else #f]) (replacement source row line)))
        (or (and styler (guard (ex [else #f]) (styler source row line))) ((mode:line-styles mode) line)))))
  (define (render projection d width height range)
    (if (not (cadr projection)) (if (zero? (car range)) (list "[Selection history unavailable]") '())
      (map (lambda (row)
             (let* ([shown (list-ref row 4)] [text (if (vector? shown) (apply string-append (vector->list shown)) shown)]
                    [visible (glyph:slice text (caddr row) (max 0 (min width (- (cadddr row) (caddr row)))))])
               (let scan ([i 0] [out #f])
                 (if (= i (string-length visible)) (or out visible)
                   (let ([n (char->integer (string-ref visible i))])
                     (if (or (< n 32) (<= 127 n 159))
                       (let ([out (or out (string-copy visible))]) (string-set! out i #\space) (scan (+ i 1) out))
                       (scan (+ i 1) out)))))))
        (list-ref projection 6))))
  (define (decorate projection d width height range)
    (if (not (cadr projection)) (list (list (list 0 0 width 1) 'ghost))
      (let* ([data (car projection)] [lines (text-control:lines (cadr data))] [frame (caddr data)]
             [selected (and (cadddr (state d)) (text-source:span (cadr projection)))])
        (apply append
          (map (lambda (row)
                 (let ([y (car row)] [r (cadr row)] [left (caddr row)] [end (min (cadddr row) (+ (caddr row) width))])
                   (let ([styles (list-ref row 5)])
                     (append
                       (if (vector? styles)
                         (let loop ([i left] [out '()])
                           (if (>= i (min end (vector-length styles))) (reverse out)
                             (let* ([style (vector-ref styles i)]
                                    [j (let run ([j (+ i 1)]) (if (and (< j (min end (vector-length styles))) (equal? style (vector-ref styles j))) (run (+ j 1)) j))])
                               (loop j (if (or (not style) (eq? style 'plain)) out (cons (list (list (- i left) y (- j i) 1) style) out)))))) '())
                       (if (and selected (<= (car (text:span-start selected)) r (car (text:span-end selected))))
                         (let* ([a (if (= r (car (text:span-start selected))) (cdr (text:span-start selected)) 0)]
                                [b (if (= r (car (text:span-end selected))) (cdr (text:span-end selected)) (+ 1 (string-length (vector-ref lines r))))]
                                [a (max 0 (- (render:column frame r a) left))]
                                [b (min width (- (render:column frame r b #t) left))])
                           (if (> b a) (list (list (list a y (- b a) 1) 'selection)) '())) '())))))
            (list-ref projection 6))))))
  (define (caret projection d width height)
    (and (cadr projection)
      (let* ([data (car projection)] [lines (text-control:lines (cadr data))])
        (text-layout:locate lines (caddr data) (and (option d 'wrap #t) (max 1 width))
          (caddr projection) (cadddr projection) (caadr projection)))))

  (define (geometry id source d reveal?)
    (let* ([f (widget:event-frame)]
           [size (if (and f (equal? id (widget:frame-id f)))
                   (cons (caddr (widget:frame-rect f)) (cadddr (widget:frame-rect f))) (mount-dimensions (mounted id)))])
      (unless size (refuse "Editor motion requires an allocated view"))
      (project (prepare id source '()) d (car size) (cdr size) reveal?)))
  (define (next-state id source d ps marked? reveal?)
    (let* ([m (mounted id)] [frame (render:prepare #f #f (text-control:lines source) (text-control:revision source) '())]
           [ps (map (lambda (p) (snap (text-control:lines source) frame p)) ps)])
      (when (mount-dimensions m)
        (let* ([proposed (descriptor:with d (list (cons 'state (append ps (list marked?))) (cons 'basis (text-control:revision source))))]
               [g (geometry id source proposed reveal?)] [wrap (and (option d 'wrap #t) (max 1 (list-ref g 4)))])
          (set! ps (list (car ps) (cadr ps) (text-layout:anchor (text-control:lines source) wrap (caddr g))))))
      (append ps (list marked?))))
  (define (publish! id source d ps marked? reveal?)
    (unless (text-control:current? id source d) (refuse "The editor view changed"))
    (interaction:set-state! head:ui-actor id (text-control:revision source) (next-state id source d ps marked? reveal?))
    (mount-group-set! (mounted id) #f))

  (edoc "Set an editor view's logical caret and selection anchor, snapping to whole graphemes. An equal pair clears mark activity. This also establishes a fresh selection when retained history is unavailable."
        (id model "mounted editor view") (caret position "active endpoint") (anchor position "fixed endpoint"))
  (define (select! id caret anchor)
    (unless (and (position? caret) (position? anchor)) (error 'select! "expected logical text positions"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let ([ps (points source d)])
        (publish! id source d (list caret anchor (if ps (caddr ps) caret)) (not (equal? caret anchor)) #t)
        (mount-goal-set! (mounted id) #f))))
  (define (adjacent lines p direction)
    (let* ([r (car p)] [c (cdr p)] [line (vector-ref lines r)]
           [edges (fold-left (lambda (out cluster) (cons (+ (car out) (car cluster)) out)) '(0) (glyph:clusters line))])
      (if (eq? direction 'left)
        (cond [(> c 0) (cons r (or (find (lambda (n) (< n c)) edges) 0))]
          [(> r 0) (cons (- r 1) (string-length (vector-ref lines (- r 1))))] [else p])
        (cond [(< c (string-length line)) (cons r (find (lambda (n) (> n c)) (reverse edges)))]
          [(< (+ r 1) (vector-length lines)) (cons (+ r 1) 0)] [else p]))))

  (edoc "Move an explicit editor caret by grapheme, displayed row, line or document endpoint. Up/down require an allocated view and retain a head-local display column. Extend preserves the selection anchor."
        (id model "editor view") (direction (one-of left right up down home end start finish) "motion")
        (extend (list-of boolean) "optional selection extension"))
  (define (move! id direction . extend)
    (unless (and (memq direction '(left right up down home end start finish)) (<= (length extend) 1) (for-all boolean? extend)) (error 'move! "invalid movement"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let* ([ps (points source d)] [lines (text-control:lines source)] [m (mounted id)] [mark? (if (pair? extend) (car extend) (cadddr (state d)))])
        (unless ps (refuse "Editor selection history is unavailable; select a fresh position"))
        (let* ([p (car ps)] [span (text-source:span ps)]
               [next (case direction
                       [(left right) (if (and (not mark?) (cadddr (state d)) (not (equal? (car ps) (cadr ps))))
                                       (if (eq? direction 'left) (text:span-start span) (text:span-end span)) (adjacent lines p direction))]
                       [(home) (cons (car p) 0)] [(end) (cons (car p) (string-length (vector-ref lines (car p))))]
                       [(start) '(0 . 0)] [(finish) (let ([r (- (vector-length lines) 1)]) (cons r (string-length (vector-ref lines r))))]
                       [else (let* ([g (geometry id source d #f)] [frame (caddr (car g))] [width (list-ref g 4)]
                                    [wrap (and (option d 'wrap #t) (max 1 width))]
                                    [goal (or (mount-goal m) (car (text-layout:locate lines frame wrap (caddr g) 0 p)))])
                               (mount-goal-set! m goal)
                               (text-layout:move lines frame wrap p (if (eq? direction 'up) -1 1) goal))])])
          (publish! id source d (list next (if mark? (cadr ps) next) (caddr ps)) mark? #t)
          (unless (memq direction '(up down)) (mount-goal-set! m #f))))))

  (define (typing-basis d)
    (list (view:generation d) (view:sequence d) (view:basis d)
      (let loop ([d d])
        (if (view:parent d) (loop (interaction:snapshot (view:parent d)))
          (list (view:generation d) (view:sequence d))))))
  (define (continues? m source d kind)
    (let ([group (mount-group m)])
      (and group (eq? (car group) kind) (= (or (view:basis d) (text-control:revision source)) (text-control:revision source))
        (equal? (cadr group) (typing-basis d)))))
  (define (replace! id source d selection replacement typing? . accepted)
    (unless (and (equal? (car selection) (cadr selection)) (equal? replacement '("")))
      (let* ([m (mounted id)] [old (text-control:basis-text source d)] [basis (or (view:basis d) (text-control:revision source))]
             [old-group (mount-group m)]
             [join? (and typing? (continues? m source d 'typing))]
             [key (if join? (caddr old-group) (list 'editor head:ui-actor id (gensym->unique-string (gensym))))]
             [document (text-source:id (text-control:mirror source))]
             [reload? (and (not join?) (document:check! head:ui-actor document))])
        (mount-group-set! m #f)
        (let ([settled? (text-control:submit! id source d old basis (text-source:span selection) replacement (list key "Edit text")
                          (list 'end)
                          (lambda (ps)
                            (let* ([mirror (text-control:mirror source)]
                                   [top (text-source:rebase (list (caddr (state d))) (text-source:changes mirror basis (text-source:revision mirror)))])
                              (next-state id (list (cons 'id (list 'buffer document))
                                               (cons 'revision (text-source:revision mirror)) (cons 'value (text-source:lines mirror))) d
                                (list (car ps) (car ps) (if top (car top) (car ps))) #f #t))))])
          (mount-goal-set! m #f)
          (when (and settled? typing? (text-control:current? id source d))
            (let ([now (interaction:snapshot id)])
              (mount-group-set! m (list 'typing (typing-basis now) key))))
          ;; Clipboard publication follows admission even when a callback has
          ;; closed the view or established a newer selection.
          (for-each (lambda (callback) (callback settled?)) accepted))
        (when reload? (document:reload! head:ui-actor document) (text-source:open! head:ui-actor document)))))

  (edoc "Insert multiline text into an explicit editor, replacing its active selection. Consecutive insertions at the unchanged resulting caret share an undo group. Stale or read-only edits refuse through the shared journal."
        (id model "editor view") (text string "inserted text"))
  (define (insert! id text)
    (insert-text! id text #t))

  (edoc "Paste text into an explicit editor as one undo action, separate from surrounding typing."
        (id model "editor view") (text string "inserted text"))
  (define (paste! id text)
    (insert-text! id text #f))
  (define (insert-text! id text typing?)
    (unless (string? text) (error 'insert! "expected text"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let ([s (state d)])
        (replace! id source d (if (cadddr s) s (list (car s) (car s)))
          (let loop ([start 0] [end 0] [out '()])
            (cond [(= end (string-length text)) (reverse (cons (substring text start end) out))]
              [(char=? (string-ref text end) #\newline) (loop (+ end 1) (+ end 1) (cons (substring text start end) out))]
              [else (loop start (+ end 1) out)])) typing?))))

  (edoc "Transfer a selected region or the rest of a line through an explicit clipboard capability. The publisher receives text and whether a preceding kill in this view can accumulate; rejected cuts never publish."
        (id model "editor view") (operation (one-of copy cut line forward backward) "transfer") (publish procedure "(text accumulate?)"))
  (define (transfer! id operation publish)
    (unless (memq operation '(copy cut line forward backward)) (error 'transfer! "invalid transfer"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let* ([s (state d)] [p (car s)] [old (text-control:basis-text source d)]
             [selection (case operation
                          [(line) (list p (let ([end (string-length (vector-ref old (car p)))])
                                            (if (< (cdr p) end) (cons (car p) end) (adjacent old p 'right))))]
                          [(forward backward)
                           (let-values ([(start end) ((if (eq? operation 'forward) expression:forward expression:backward) old p)])
                             (unless start (refuse "No expression in that direction"))
                             (list p (if (eq? operation 'forward) end start)))]
                          [else s])]
             [span (text-source:span selection)] [m (mounted id)] [join? (continues? m source d 'kill)])
        (unless (or (memq operation '(line forward backward)) (cadddr s)) (refuse "The mark is not set"))
        (unless (text:span-empty? span)
          (let ([text (text:to-string (list->vector (text:extract old span)) #f)])
            (if (eq? operation 'copy)
              (let* ([changes (text-source:changes (text-control:mirror source) (or (view:basis d) (text-control:revision source)) (text-control:revision source))]
                     [kept (and changes (fold-left (lambda (span change) (and span (text:rebase-span span change))) span changes))]
                     [ps (points source d)])
                (unless (and kept ps) (refuse "The selected text changed; select it again"))
                (publish! id source d (list (car ps) (car ps) (caddr ps)) #f #f)
                (publish text #f))
              (replace! id source d selection '("") #f
                (lambda (settled?)
                  (let ([after (and settled? (typing-basis (interaction:snapshot id)))])
                    (publish text join?)
                    (when (and after (text-control:current? id source d)
                            (equal? after (typing-basis (interaction:snapshot id))))
                      (mount-group-set! m (list 'kill after #f))))))))))))

  (edoc "Operate on Scheme expressions in an explicit editor, sharing immutable source analysis across views. Motions and marks use current mirrored text; transposition is admitted against its captured revision."
        (id model "editor view") (operation (one-of forward backward up down next previous start end mark form transpose) "expression operation"))
  (define (expression! id operation)
    (let-values ([(source d) (text-control:context id 'editor)])
      (if (eq? operation 'transpose)
        (let* ([old (text-control:basis-text source d)] [p (car (state d))])
          (let-values ([(as ae) (expression:backward old p)] [(bs be) (expression:forward old p)])
            (unless (and as bs (not (equal? as bs))) (refuse "No two expressions around the caret"))
            (let-values ([(lines trailing?) (text:from-string (string-append (expression:text old bs be)
                                                                (expression:text old ae bs) (expression:text old as ae)))])
              (replace! id source d (list as be) (append (vector->list lines) (if trailing? '("") '())) #f))))
        (let* ([ps (points source d)] [lines (text-control:lines source)] [marked? (cadddr (state d))])
          (unless ps (refuse "Editor selection history is unavailable"))
          (let* ([p (car ps)] [anchor (cadr ps)]
                 [from (if (and (eq? operation 'mark) marked? (or (< (car p) (car anchor))
                                                                (and (= (car p) (car anchor)) (< (cdr p) (cdr anchor))))) anchor p)])
            (let-values ([(a b)
                          (case operation
                            [(forward mark) (expression:forward lines from)] [(backward) (expression:backward lines p)]
                            [(up) (expression:container lines p)] [(next) (expression:next-list lines p)]
                            [(previous) (expression:previous-list lines p)] [(form) (expression:top-level lines p)]
                            [(down) (values (expression:down lines p) #f)] [(start) (values (expression:form-start lines p) #f)]
                            [(end) (values (expression:form-end lines p) #f)]
                            [else (error 'expression! "invalid operation" operation)])])
              (unless a (refuse "No expression in that direction"))
              (let ([next (if (memq operation '(forward next)) b a)])
                (publish! id source d
                  (list (if (eq? operation 'mark) p next)
                    (cond [(memq operation '(mark form)) b] [marked? anchor] [else next]) (caddr ps))
                  (or marked? (and (memq operation '(mark form)) #t)) #t)
                (mount-goal-set! (mounted id) #f))))))))

  (edoc "Page an allocated editor by a fraction of its height, retaining mark activity and the desired display column. At an already reached edge, move the caret to that edge."
        (id model "editor view") (direction integer "negative up, positive down") (fraction integer "positive page divisor"))
  (define (page! id direction fraction)
    (unless (and (integer? direction) (exact? direction) (not (zero? direction))
              (integer? fraction) (exact? fraction) (> fraction 0)) (error 'page! "invalid page direction or divisor"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let* ([g (geometry id source d #f)] [ps (cadr g)] [lines (text-control:lines source)]
             [frame (caddr (car g))] [wrap (and (option d 'wrap #t) (max 1 (list-ref g 4)))] [m (mounted id)])
        (unless ps (refuse "Editor selection history is unavailable"))
        (let ([goal (or (mount-goal m) (car (text-layout:locate lines frame wrap (caddr g) 0 (car ps))))]
              [marked? (cadddr (state d))])
          (let-values ([(top caret) (text-layout:page lines frame wrap 0 (list-ref g 5) (caddr g) goal direction fraction)])
            (publish! id source d (list caret (if marked? (cadr ps) caret) (text-layout:anchor lines wrap top)) marked? #f)
            (mount-goal-set! m goal))))))

  (edoc "Delete an editor's active selection, or an adjacent grapheme or newline."
        (id model "editor view") (direction (one-of backward forward) "deletion direction"))
  (define (delete! id direction)
    (unless (memq direction '(backward forward)) (error 'delete! "invalid deletion direction"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let* ([s (state d)] [p (car s)] [old (text-control:basis-text source d)])
        (replace! id source d (if (and (cadddr s) (not (equal? p (cadr s)))) s
                                (list p (adjacent old p (if (eq? direction 'backward) 'left 'right)))) '("") #f))))

  (edoc "Move an editor's source journal and rebase its view without changing any other selection."
        (id model "editor view") (direction symbol "undo or redo") (scope any "undo actor scope"))
  (define (history! id direction scope)
    (let-values ([(source d) (text-control:context id 'editor)])
      (mount-group-set! (mounted id) #f) (mount-goal-set! (mounted id) #f)
      (text-control:history! id source d direction scope (list-head (state d) 3)
        (lambda (ps) (list (car ps) (car ps) (caddr ps) #f)))))

  (edoc "Set or clear an editor's mark. Setting it anchors at the current caret; clearing collapses the selection."
        (id model "editor view") (active boolean "mark activity"))
  (define (set-mark! id active)
    (unless (boolean? active) (error 'set-mark! "expected a boolean"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let ([ps (points source d)])
        (unless ps (refuse "Editor selection history is unavailable"))
        (publish! id source d (list (car ps) (car ps) (caddr ps)) active #t))))

  (edoc "Scroll an editor viewport by displayed rows without changing its caret or selection. Uses the mounted geometry and persists only a logical top anchor."
        (id model "editor view") (rows integer "positive down, negative up"))
  (define (scroll! id rows)
    (unless (and (integer? rows) (exact? rows)) (error 'scroll! "expected displayed rows"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let* ([g (geometry id source d #f)] [ps (cadr g)] [data (car g)] [lines (text-control:lines source)]
             [width (and (option d 'wrap #t) (max 1 (list-ref g 4)))])
        (unless ps (refuse "Editor selection history is unavailable"))
        (let ([top (text-layout:move lines (caddr data) width (text-layout:anchor lines width (caddr g)) rows 0)])
          (publish! id source d (list (car ps) (cadr ps) top) (cadddr (state d)) #f)
          (- rows (text-layout:distance lines width (caddr g) top))))))

  (define (pointer-bindings frame x y)
    (let* ([g (widget:frame-data frame)] [data (car g)] [d (widget:frame-descriptor frame)] [ps (cadr g)])
      (if (not ps) '()
        (let* ([lines (text-control:lines (cadr data))] [width (caddr (widget:frame-rect frame))]
               [p (snap lines (caddr data) (text-layout:hit lines (caddr data) (and (option d 'wrap #t) (max 1 width)) (caddr g) (cadddr g) (max 0 x) (max 0 y)))]
               [id (widget:frame-id frame)] [current (points (cadr data) (interaction:snapshot id))])
          (append (list (list '(click primary ()) (keymap:call select! id p p))
                    (list '(wheel up ()) (keymap:call scroll! id -3)) (list '(wheel down ()) (keymap:call scroll! id 3)))
            (if current
              (list (list '(click primary (shift)) (keymap:call select! id p (cadr current)))
                (list '(drag primary ()) (keymap:call select! id p (cadr current)))) '()))))))
  (define (event! id source d event)
    (case (car event)
      [(text) (if (eq? (caddr event) 'paste)
                (paste! id (string:join (tty:paste-lines (cadr event)) "\n"))
                (insert! id (cadr event))) #t]
      [(cancel) (when (equal? dragging id) (set! dragging #f)) #t]
      [(pointer)
       (cond [(and (eq? (cadr event) 'release) (equal? dragging id)) (set! dragging #f) #t]
         [else (and (eq? (caddr event) 'primary)
                 (or (eq? (cadr event) 'press) (and (eq? (cadr event) 'move) (equal? dragging id)))
                 (let* ([extend? (or (eq? (cadr event) 'move) (memq 'shift (cadddr event)))]
                        [binding (assoc (if extend? '(click primary (shift)) '(click primary ()))
                                   (pointer-bindings (widget:event-frame) (list-ref event 4) (list-ref event 5)))])
                   (and binding (begin (keymap:run! (cadr binding))
                                  (when (eq? (cadr event) 'press) (set! dragging id) (widget:capture! id)) #t))))])]
      [else #f]))

  (edoc "Install the editor widget definition and explicit command bindings. History and clipboard policy are supplied by the canonical edit API."
        (commands list "named command procedures"))
  (define (register! commands)
    (widget:register! 'editor 1
      (list (cons 'prepare prepare) (cons 'viewport viewport) (cons 'render render) (cons 'decorate decorate) (cons 'caret caret)
        (cons 'service service!) (cons 'release release!) (cons 'focus #t) (cons 'contexts '(widget-editor))
        (cons 'event event!) (cons 'pointer-bindings pointer-bindings)
        (cons 'actions (append (list (cons 'insert insert!) (cons 'delete delete!) (cons 'select select!) (cons 'move move!) (cons 'scroll scroll!) (cons 'set-mark set-mark!)) commands))))
    (for-each (lambda (b) (keymap:bind-default! 'widget-editor (car b)
                            (if (caddr b) (keymap:call move! widget:target (cadr b) #t) (keymap:call move! widget:target (cadr b)))))
      '(("LEFT" left #f) ("RIGHT" right #f) ("UP" up #f) ("DOWN" down #f)
        ("C-b" left #f) ("C-f" right #f) ("C-p" up #f) ("C-n" down #f)
        ("C-a" home #f) ("C-e" end #f) ("M-<" start #f) ("M->" finish #f)
        ("HOME" home #f) ("END" end #f) ("C-HOME" start #f) ("C-END" finish #f)
        ("S-LEFT" left #t) ("S-RIGHT" right #t) ("S-UP" up #t) ("S-DOWN" down #t)))
    (keymap:bind-default! 'widget-editor "BACKSPACE" (keymap:call delete! widget:target 'backward))
    (keymap:bind-default! 'widget-editor "DELETE" (keymap:call delete! widget:target 'forward))
    (keymap:bind-default! 'widget-editor "RET" (keymap:call insert! widget:target "\n"))
    (keymap:bind-default! 'widget-editor "C-@" (keymap:call set-mark! widget:target #t))
    (keymap:bind-default! 'widget-editor "C-g" (keymap:call set-mark! widget:target #f))
    (for-each (lambda (b) (keymap:bind-default! 'widget-editor (car b)
                            (keymap:call (apply (cdr (assq (cadr b) commands)) (cons widget:target (cddr b))))))
      '(("C-_" undo) ("C-M-_" redo) ("C-k" kill-line) ("C-w" kill-region) ("M-w" copy-region) ("C-y" yank)
        ("PAGEUP" page -1 1) ("PAGEDOWN" page 1 1) ("M-v" page -1 1) ("C-v" page 1 1)
        ("C-M-f" forward-expression) ("C-M-b" backward-expression) ("C-M-u" up-expression) ("C-M-d" down-expression)
        ("C-M-n" next-list) ("C-M-p" previous-list) ("C-M-a" beginning-of-form) ("C-M-e" end-of-form)
        ("C-M-@" mark-expression) ("C-M-h" mark-form) ("C-M-t" transpose-expressions)
        ("C-M-k" kill-expression) ("C-M-BACKSPACE" backward-kill-expression)))))
