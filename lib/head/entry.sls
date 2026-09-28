;; A single-line control over authored text, with selection owned by its view.
(import (only (foundation edoc) elibrary))
(elibrary (head entry)
  (export delete! init! insert! move! redo! select! undo!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation text) text:)
          (prefix (only (head edit) undo-scope) edit:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:)
          (prefix (head widget) widget:)
          (prefix (state view) view:)
          (prefix (sys glyph) glyph:))

  (define (refuse message)
    (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (source-buffer source)
    (let ([id (and source (cdr (assq 'id source)))])
      (unless (and (pair? id) (eq? (car id) 'buffer))
        (error 'entry "expected a text buffer source" id))
      (or (head:buffer-of-store-id (cadr id)) (error 'entry "source is unavailable" id))))
  (define (revision source) (cdr (assq 'revision source)))
  (define (source-lines source) (cdr (assq 'value source)))
  (define (single-line? lines) (= (vector-length lines) 1))
  (define (state d)
    (let ([s (view:state d)])
      (unless (and (list? s) (= (length s) 2)
                (for-all (lambda (p) (and (pair? p) (integer? (car p)) (exact? (car p)) (>= (car p) 0)
                                       (integer? (cdr p)) (exact? (cdr p)) (>= (cdr p) 0))) s))
        (error 'entry "expected (caret anchor) text positions" s))
      s))
  (define (changes source basis)
    (let-values ([(lines rev changes) (head:snapshot-since (source-buffer source) basis)])
      (and changes (<= basis (revision source))
        (map caddr (filter (lambda (row) (<= (car row) (revision source))) changes)))))
  (define (span s)
    (let ([a (car s)] [b (cadr s)])
      (if (or (< (car a) (car b)) (and (= (car a) (car b)) (<= (cdr a) (cdr b))))
        (text:make-span (car a) (cdr a) (car b) (cdr b))
        (text:make-span (car b) (cdr b) (car a) (cdr a)))))
  (define (selection source d)
    (let ([steps (changes source (or (view:basis d) (revision source)))])
      (and steps (map (lambda (p) (fold-left text:rebase-position p steps)) (state d)))))
  (define (data id source inputs)
    (source-buffer source)
    (let* ([lines (source-lines source)] [line (if (single-line? lines) (vector-ref lines 0) "")])
      (list source line
        (let loop ([parts (glyph:clusters line)] [chars 0] [cells 0] [out '((0 . 0))])
          (if (null? parts) (reverse out)
            (let ([chars (+ chars (caar parts))] [cells (+ cells (cdar parts))])
              (loop (cdr parts) chars cells (cons (cons chars cells) out))))))))
  (define (edge edges value which)
    (let loop ([rest (cdr edges)] [previous (car edges)])
      (if (or (null? rest) (> (which (car rest)) value)) previous
        (loop (cdr rest) (car rest)))))
  (define (project data d)
    (let ([s (selection (car data) d)])
      (and (single-line? (source-lines (car data))) s
        (for-all (lambda (p) (zero? (car p))) s)
        (map (lambda (p) (edge (caddr data) (cdr p) car)) s))))
  (define (offset points width) (max 0 (- (cdar points) (max 0 (- width 1)))))
  (define (message source)
    (if (single-line? (source-lines source)) "[Selection history unavailable]" "[Single-line field: source has multiple lines]"))
  (define (render data d width height range)
    (if (or (> (car range) 0) (zero? (cdr range))) '()
      (let ([points (project data d)])
        (list (if points (glyph:slice (cadr data) (offset points width) width) (message (car data)))))))
  (define (decorate data d width height range)
    (let ([points (project data d)])
      (if points
        (let* ([start (offset points width)] [a (- (cdar points) start)] [b (- (cdadr points) start)]
               [left (max 0 (min a b))] [right (min width (max a b))])
          (if (> right left) (list (list (list left 0 (- right left) 1) 'selection)) '()))
        (list (list (list 0 0 width 1) 'ghost)))))
  (define (caret data d width height)
    (let ([points (project data d)])
      (and points (> width 0) (> height 0) (cons (- (cdar points) (offset points width)) 0))))
  (define (context id)
    (let-values ([(source d inputs) (widget:context id)])
      (unless (and d (eq? (view:kind d) 'entry) (= (view:schema d) 1)) (error 'entry "expected an entry view" id))
      (source-buffer source)
      (values source d)))

  (edoc "Select a range in an entry's text source; caret and anchor are character indices, snapped to whole graphemes."
        (id model "entry view") (caret integer "active end") (anchor integer "fixed end"))
  (define (select! id caret anchor)
    (unless (and (integer? caret) (exact? caret) (>= caret 0) (integer? anchor) (exact? anchor) (>= anchor 0))
      (error 'select! "expected nonnegative character indices" caret anchor))
    (let-values ([(source d) (context id)])
      (unless (single-line? (source-lines source)) (refuse "Entry requires a single-line source"))
      (let ([edges (caddr (data id source '()))])
        (interaction:set-state! head:ui-actor id (revision source)
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
                       (and (single-line? (source-lines source)) (memq direction '(home end))
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

  (define (basis-text source d)
    (let ([steps (changes source (or (view:basis d) (revision source)))])
      (unless steps (refuse "Entry selection history is unavailable"))
      ;; Reconstruct the declared basis, not a new intent over changed text.
      ;; The store checks the original span against every intervening edit.
      (fold-left (lambda (lines delta)
                   (let ([inverse (text:invert-delta delta)])
                     (let-values ([(lines ignored) (text:apply-edit lines (text:delta-span inverse) (text:delta-inserted inverse))]) lines)))
        (source-lines source) (reverse steps))))
  (define (replace! id source d selection replacement)
    (unless (single-line? (source-lines source)) (refuse "Entry requires a single-line source"))
    (let ([b (source-buffer source)] [basis (or (view:basis d) (revision source))]
          [old (basis-text source d)])
      (unless (single-line? old) (refuse "Entry selection refers to a multiline source"))
      (unless (and (equal? (car selection) (cadr selection)) (string=? replacement ""))
        (head:store-edit! b (span selection) (list replacement) #f
          (list (cons (lambda (point revision)
                        (interaction:set-state! head:ui-actor id revision (list point point))) 'end))
          (list old (head:buffer-store-id b) basis)))))

  (edoc "Insert text once, replacing the entry selection through the shared edit journal. Multiline input is refused whole."
        (id model "entry view") (text string "committed text"))
  (define (insert! id text)
    (unless (and (string? text) (not (exists (lambda (c) (memv c '(#\newline #\return))) (string->list text))))
      (refuse "Entry does not accept multiline text"))
    (let-values ([(source d) (context id)]) (replace! id source d (state d) text)))

  (edoc "Delete the entry selection, or a whole adjacent grapheme."
        (id model "entry view") (direction (one-of backward forward all) "adjacent grapheme or all text"))
  (define (delete! id direction)
    (unless (memq direction '(backward forward all)) (error 'delete! "invalid direction" direction))
    (let-values ([(source d) (context id)])
      (if (or (eq? direction 'all) (equal? (car (state d)) (cadr (state d))))
        (let* ([old (basis-text source d)]
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
      (let* ([b (source-buffer source)] [basis (or (view:basis d) (revision source))])
        (let-values ([(status detail) (head:store-history! b direction scope)])
          (when (eq? status 'applied)
            (let-values ([(text revision steps) (head:snapshot-since b basis)])
              (when steps
                (interaction:set-state! head:ui-actor id revision
                  (map (lambda (p) (fold-left text:rebase-position p (map caddr steps))) (state d))))))
          (values status detail)))))

  (edoc "Undo an entry's source using the editor's undo-scope, with an optional explicit mine, all or (actor identity) scope."
        (id model "entry view") (scope (list-of any) "scope override, at most one"))
  (define (undo! id . scope)
    (unless (<= (length scope) 1) (error 'undo! "expected at most one scope" scope))
    (history! id 'undo (if (pair? scope) (car scope) (edit:undo-scope))))

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
               [steps (changes (car data) (or (view:basis d) (revision (car data))))]
               [extend? (and current steps (fold-left (lambda (s delta) (and s (text:rebase-span s delta))) (span (state d)) steps))])
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
        (cons 'measure (lambda (data d axis cross measure) (if (eq? axis 'y) '(1 1) (list 1 (max 1 (cdr (car (reverse (caddr data)))))))))
        (cons 'focus #t) (cons 'contexts '(widget-entry)) (cons 'event event!) (cons 'pointer-bindings pointer-bindings)
        (cons 'actions (list (cons 'insert insert!) (cons 'select select!) (cons 'move move!)
                         (cons 'delete delete!) (cons 'undo undo!) (cons 'redo redo!)))))
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
