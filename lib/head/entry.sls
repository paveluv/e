;; A single-line control over authored text, with selection owned by its view.
(import (only (foundation edoc) elibrary))
(elibrary (head entry)
  (export delete! init! insert! move! redo! register-policy! register-presentation! select! set-text! undo!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation text) text:)
          (prefix (only (head head) ui-actor) head:)
          (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:)
          (prefix (head text-control) text-control:)
          (prefix (head text-source) text-source:)
          (prefix (head widget) widget:)
          (prefix (state view) view:)
          (prefix (sys glyph) glyph:))

  (define (refuse message)
    (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (single-line? lines) (= (vector-length lines) 1))
  (define (state d)
    (let ([s (view:state d)])
      (unless (and (list? s) (= (length s) 2)
                (for-all (lambda (p) (and (pair? p) (integer? (car p)) (exact? (car p)) (>= (car p) 0)
                                       (integer? (cdr p)) (exact? (cdr p)) (>= (cdr p) 0))) s))
        (error 'entry "expected (caret anchor) text positions" s))
      s))
  (define (changes source basis)
    (text-source:changes (text-control:mirror source) basis (text-control:revision source)))
  (define (selection source d)
    (text-source:rebase (state d) (changes source (or (view:basis d) (text-control:revision source)))))
  (define presentations (kernel:make-registry car))
  (define policies (kernel:make-registry car))
  (define presentation-changes (kernel:registry-observe! presentations (lambda (removed added) (widget:invalidate!))))

  (edoc "Register a pure text editing policy. Given proposed single-line text and a character caret, return (text caret). A view selects it with its policy (name schema) option. This operates on logical text, independent of display projection."
        (name symbol "policy name") (schema integer "positive version") (normalize procedure "pure text and caret normalization"))
  (define (register-policy! name schema normalize)
    (unless (and (symbol? name) (integer? schema) (exact? schema) (> schema 0) (procedure? normalize))
      (error 'register-policy! "invalid entry policy"))
    (kernel:registry-add! policies (cons (list name schema) normalize)))

  (edoc "Register a pure entry projection: (text context) returns one (display roles) pair per source grapheme. Roles are symbols. Painting, caret, selection and mouse hits share this mapping; edits address the original text."
        (name symbol "presentation name") (schema integer "positive version") (project procedure "pure grapheme formatter"))
  (define (register-presentation! name schema project)
    (unless (and (symbol? name) (integer? schema) (exact? schema) (> schema 0) (procedure? project))
      (error 'register-presentation! "invalid entry presentation"))
    (kernel:registry-add! presentations (cons (list name schema) project)))

  (define (data id source inputs)
    (text-control:mirror source)
    (let* ([lines (text-control:lines source)] [line (if (single-line? lines) (vector-ref lines 0) "")]
           [profile (assq 'presentation (view:options (interaction:snapshot id)))]
           [definition (and profile (kernel:registry-find presentations (lambda (p) (equal? (cdr profile) (car p)))))]
           [context (assq 'context inputs)] [parts (glyph:clusters line)]
           [display (and definition ((cdr definition) line (if (and context (eq? (cadr context) 'ready)) (caddr context) '())))])
      (when (and profile (not definition)) (error 'entry "entry presentation is unavailable" (cdr profile)))
      (when (and definition
              (not (and (list? display) (= (length display) (length parts))
                     (for-all (lambda (p) (and (list? p) (= (length p) 2) (string? (car p))
                                            (positive? (string-length (car p))) (list? (cadr p)) (for-all symbol? (cadr p)))) display))))
        (error 'entry "invalid grapheme projection"))
      (let loop ([parts parts] [display display] [chars 0] [cells 0] [edges '((0 . 0))] [out '()] [spans '()])
        (if (null? parts) (list source (apply string-append (reverse out)) (reverse edges) (reverse spans))
          (let* ([end (+ chars (caar parts))] [text (if display (caar display) (substring line chars end))]
                 [width (glyph:cells text)] [roles (if display (cadar display) '())])
            (loop (cdr parts) (and display (cdr display)) end (+ cells width)
              (cons (cons end (+ cells width)) edges) (cons text out)
              (if (null? roles) spans (cons (list cells (+ cells width) roles) spans))))))))
  (define (edge edges value which)
    (let loop ([rest (cdr edges)] [previous (car edges)])
      (if (or (null? rest) (> (which (car rest)) value)) previous
        (loop (cdr rest) (car rest)))))
  (define (project data d)
    (let ([s (selection (car data) d)])
      (and (single-line? (text-control:lines (car data))) s
        (for-all (lambda (p) (zero? (car p))) s)
        (map (lambda (p) (edge (caddr data) (cdr p) car)) s))))
  (define (offset points width) (max 0 (- (cdar points) (max 0 (- width 1)))))
  (define (message source)
    (if (single-line? (text-control:lines source)) "[Selection history unavailable]" "[Single-line field: source has multiple lines]"))
  (define (render data d width height range)
    (if (or (> (car range) 0) (zero? (cdr range))) '()
      (let ([points (project data d)])
        (list (if points (glyph:slice (cadr data) (offset points width) width) (message (car data)))))))
  (define (decorate data d width height range)
    (let ([points (project data d)])
      (if points
        (let* ([start (offset points width)] [a (- (cdar points) start)] [b (- (cdadr points) start)]
               [left (max 0 (min a b))] [right (min width (max a b))])
          (append
            (filter values
              (map (lambda (span)
                     (let ([a (max 0 (- (car span) start))] [b (min width (- (cadr span) start))])
                       (and (< a b) (list (list a 0 (- b a) 1) (caddr span))))) (cadddr data)))
            (if (> right left) (list (list (list left 0 (- right left) 1) 'selection)) '())))
        (list (list (list 0 0 width 1) 'ghost)))))
  (define (caret data d width height)
    (let ([points (project data d)])
      (and points (> width 0) (> height 0) (cons (- (cdar points) (offset points width)) 0))))
  (define (context id)
    (text-control:context id 'entry))

  (edoc "Select a range in an entry's text source; caret and anchor are character indices, snapped to whole graphemes."
        (id model "entry view") (caret integer "active end") (anchor integer "fixed end"))
  (define (select! id caret anchor)
    (unless (and (integer? caret) (exact? caret) (>= caret 0) (integer? anchor) (exact? anchor) (>= anchor 0))
      (error 'select! "expected nonnegative character indices" caret anchor))
    (let-values ([(source d) (context id)])
      (unless (single-line? (text-control:lines source)) (refuse "Entry requires a single-line source"))
      (let ([edges (caddr (data id source '()))])
        (interaction:set-state! head:ui-actor id (text-control:revision source)
          (map (lambda (n) (cons 0 (car (edge edges n car)))) (list caret anchor))))))

  (edoc "Move an entry caret by grapheme or to an endpoint, optionally extending its selection."
        (id model "entry view") (direction (one-of left right home end) "motion")
        (extend (list-of boolean) "extend selection, at most one"))
  (define (move! id direction . extend)
    (unless (and (memq direction '(left right home end)) (<= (length extend) 1) (for-all boolean? extend))
      (error 'move! "invalid movement" direction extend))
    (let-values ([(source d) (context id)])
      (let* ([data (data id source '())]
             [points (or (project data d)
                       (and (single-line? (text-control:lines source)) (memq direction '(home end))
                         (not (and (pair? extend) (car extend))) '((0 . 0) (0 . 0))))]
             [edges (map car (caddr data))])
        (unless points (refuse "Entry selection history is unavailable"))
        (let* ([a (caar points)] [b (caadr points)] [selecting? (and (pair? extend) (car extend))]
               [next (case direction
                       [(home) 0] [(end) (car (reverse edges))]
                       [(left) (if (and (not selecting?) (not (= a b))) (min a b)
                                 (fold-left (lambda (previous n) (if (< n a) n previous)) 0 edges))]
                       [(right) (if (and (not selecting?) (not (= a b))) (max a b)
                                  (or (find (lambda (n) (> n a)) edges) a))])])
          (select! id next (if selecting? b next))))))

  (define (submit! id source d old basis span replacement context desired)
    (text-control:submit! id source d old basis span replacement context (list desired)
      (lambda (points) (list (car points) (car points)))))
  (define (replace! id source d selection replacement)
    (unless (single-line? (text-control:lines source)) (refuse "Entry requires a single-line source"))
    (let ([basis (or (view:basis d) (text-control:revision source))]
          [old (text-control:basis-text source d)])
      (unless (single-line? old) (refuse "Entry selection refers to a multiline source"))
      (unless (and (equal? (car selection) (cadr selection)) (string=? replacement ""))
        (let* ([profile (assq 'policy (view:options d))]
               [policy (and profile (kernel:registry-find policies (lambda (p) (equal? (cdr profile) (car p)))))]
               [proposed (and profile
                              (let-values ([(lines delta) (text:apply-edit old (text-source:span selection) (list replacement))])
                                (unless policy (error 'entry "entry policy is unavailable" (cdr profile)))
                                (let* ([text (vector-ref lines 0)] [caret (cdr (text:delta-new-end delta))]
                                       [next ((cdr policy) text caret)])
                                  (unless next (error 'entry "invalid normalized text or caret"))
                                  (and (not (equal? next (list text caret))) next))))])
          (when (and proposed
                     (not (and (list? proposed) (= (length proposed) 2) (string? (car proposed))
                            (not (exists (lambda (c) (memv c '(#\newline #\return))) (string->list (car proposed))))
                            (integer? (cadr proposed)) (exact? (cadr proposed)) (<= 0 (cadr proposed) (string-length (car proposed))))))
            (error 'entry "invalid normalized text or caret"))
          (submit! id source d old basis
            (if proposed (text:make-span 0 0 0 (string-length (vector-ref old 0))) (text-source:span selection))
            (list (if proposed (car proposed) replacement))
            (and proposed (list #f "Edit entry" (cons 'revision basis)))
            (if proposed (cons 0 (cadr proposed)) 'end))))))

  (edoc "Insert text once, replacing the entry selection through the shared edit journal. Multiline input is refused whole."
        (id model "entry view") (text string "committed text"))
  (define (insert! id text)
    (unless (and (string? text) (not (exists (lambda (c) (memv c '(#\newline #\return))) (string->list text))))
      (refuse "Entry does not accept multiline text"))
    (let-values ([(source d) (context id)]) (replace! id source d (state d) text)))

  (edoc "Replace an entry's whole text as one undoable edit and move its caret to the end. An optional exact source revision refuses stale asynchronous proposals, including edits at the old endpoints."
        (id model "entry view") (text string "single-line replacement") (expected (list-of integer) "optional exact source revision"))
  (define (set-text! id text . expected)
    (unless (and (string? text) (not (exists (lambda (c) (memv c '(#\newline #\return))) (string->list text))) (<= (length expected) 1))
      (error 'set-text! "expected single-line text and at most one revision"))
    (let-values ([(source d) (context id)])
      (unless (single-line? (text-control:lines source)) (refuse "Entry requires a single-line source"))
      (when (and (pair? expected) (not (equal? (car expected) (text-control:revision source)))) (refuse "Entry text changed"))
      (let* ([line (vector-ref (text-control:lines source) 0)] [rev (text-control:revision source)])
        (if (string=? line text) (select! id (string-length text) (string-length text))
          (submit! id source d (text-control:lines source) rev
            (text:make-span 0 0 0 (string-length line)) (list text)
            (list #f "Replace entry text" (cons 'revision rev)) 'end)))))

  (edoc "Delete the entry selection, or a whole adjacent grapheme."
        (id model "entry view") (direction (one-of backward forward all) "adjacent grapheme or all text"))
  (define (delete! id direction)
    (unless (memq direction '(backward forward all)) (error 'delete! "invalid direction" direction))
    (let-values ([(source d) (context id)])
      (if (or (eq? direction 'all) (equal? (car (state d)) (cadr (state d))))
        (let* ([old (text-control:basis-text source d)]
               [line (and (single-line? old) (vector-ref old 0))])
          (unless line (refuse "Entry selection refers to a multiline source"))
          (let* ([edges (fold-left (lambda (out cluster) (cons (+ (car out) (car cluster)) out)) '(0) (glyph:clusters line))]
                 [at (cdr (car (state d)))]
                 [next (if (eq? direction 'backward)
                         (or (find (lambda (n) (< n at)) edges) 0)
                         (or (find (lambda (n) (> n at)) (reverse edges)) at))])
            (replace! id source d (if (eq? direction 'all) (list '(0 . 0) (cons 0 (string-length line)))
                                    (list (cons 0 at) (cons 0 next))) "")))
        (replace! id source d (state d) ""))))

  (define (history! id direction scope)
    (let-values ([(source d) (context id)])
      (text-control:history! id source d direction scope (state d) values)))

  (edoc "Undo an entry's source using the editor's undo-scope, with an optional explicit mine, all or (actor identity) scope."
        (id model "entry view") (scope (list-of any) "scope override, at most one"))
  (define (undo! id . scope)
    (unless (<= (length scope) 1) (error 'undo! "expected at most one scope" scope))
    (history! id 'undo (if (pair? scope) (car scope) (text-control:undo-scope))))

  (edoc "Redo an entry's source through the editor's shared undo journal."
        (id model "entry view"))
  (define (redo! id)
    (history! id 'redo 'mine))

  (define dragging #f)
  (define (pointer-bindings f x y)
    (let* ([id (widget:frame-id f)] [data (widget:frame-data f)]
           [points (project data (widget:frame-descriptor f))])
      (if (not points) '()
        (let* ([at (car (edge (caddr data) (+ x (offset points (caddr (widget:frame-rect f)))) cdr))]
               [d (interaction:snapshot id)] [current (project data d)]
               [steps (changes (car data) (or (view:basis d) (text-control:revision (car data))))]
               [extend? (and current steps (fold-left (lambda (s delta) (and s (text:rebase-span s delta))) (text-source:span (state d)) steps))])
          (append (list (list '(click primary ()) (keymap:call select! id at at)))
            (if extend?
              (map (lambda (gesture) (list gesture (keymap:call select! id at (caadr current)))) '((click primary (shift)) (drag primary ()))) '()))))))
  (define (event! id source d event)
    (case (car event)
      [(text) (insert! id (cadr event)) #t]
      [(cancel) (when (equal? dragging id) (set! dragging #f)) #t]
      [(pointer)
       (cond [(and (eq? (cadr event) 'release) (equal? dragging id)) (set! dragging #f) #t]
         [else (and (eq? (caddr event) 'primary)
                 (or (eq? (cadr event) 'press) (and (eq? (cadr event) 'move) (equal? dragging id)))
                 (let* ([extend? (or (eq? (cadr event) 'move) (memq 'shift (cadddr event)))]
                        [binding (assoc (if extend? '(click primary (shift)) '(click primary ()))
                                   (pointer-bindings (widget:event-frame) (list-ref event 4) (list-ref event 5)))])
                   (when (and extend? (not binding)) (refuse "Entry selection changed during the gesture"))
                   (and binding
                     (begin (keymap:run! (cadr binding))
                       (when (eq? (cadr event) 'press) (widget:capture! id) (set! dragging id)) #t))))])]
      [else #f]))

  (edoc "Register the single-line entry definition and its ordinary keymap bindings.")
  (define (init!)
    (widget:register! 'entry 1
      (list (cons 'prepare data) (cons 'render render) (cons 'decorate decorate) (cons 'caret caret)
        (cons 'measure (lambda (data d axis cross measure) (if (eq? axis 'y) '(1 1) (list 1 (+ 1 (cdr (car (reverse (caddr data)))))))))
        (cons 'focus #t) (cons 'contexts '(widget-entry)) (cons 'event event!) (cons 'pointer-bindings pointer-bindings)
        (cons 'actions (list (cons 'insert insert!) (cons 'select select!) (cons 'move move!)
                         (cons 'delete delete!) (cons 'set-text set-text!) (cons 'undo undo!) (cons 'redo redo!)))))
    (for-each (lambda (binding)
                (keymap:bind-default! 'widget-entry (car binding)
                  (keymap:call move! widget:target (cadr binding) (caddr binding))))
      '(("LEFT" left #f) ("RIGHT" right #f) ("HOME" home #f) ("END" end #f)
        ("S-LEFT" left #t) ("S-RIGHT" right #t) ("S-HOME" home #t) ("S-END" end #t)))
    (keymap:bind-default! 'widget-entry "BACKSPACE" (keymap:call delete! widget:target 'backward))
    (keymap:bind-default! 'widget-entry "DELETE" (keymap:call delete! widget:target 'forward))
    (keymap:bind-default! 'widget-entry "C-_" (keymap:call undo! widget:target))
    (keymap:bind-default! 'widget-entry "C-M-_" (keymap:call redo! widget:target)))
)
