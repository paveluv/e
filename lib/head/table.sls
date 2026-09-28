;; Virtual collections and shared column rules. Legacy picker formatting
;; remains here until those apps adopt the composable controls.
(import (only (foundation edoc) elibrary))
(elibrary (head table)
  (export activate! create! cycle-sort heading init! layout less? make move! select! set-columns! sort-by! toggle-sort!)
  (import (chezscheme) (prefix (core row) row:) (prefix (foundation string) string:)
          (prefix (head head) head:) (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head layout) layout:) (prefix (head range) range:) (prefix (head widget) widget:)
          (prefix (state collection) collection:) (prefix (state model) model:) (prefix (state view) view:)
          (prefix (sys glyph) glyph:))

  (edoc "A table's columns: their headings, minimum widths, which column identifies a row, which may be dropped when narrow, and how cells align."
        (headings vector "the column headings")
        (minimum vector "the minimum width of each column")
        (keep integer "the column kept whatever the width")
        (drop list "the columns to drop first, in order")
        (alignment vector "text, right or tail per column"))
  (define-record-type (table make table?)
    (fields headings minimum keep drop alignment))

  (edoc "The sort keys after a column's heading is pressed: ascending, then descending, then off; direction changes keep priority."
        (keys list "(column . descending?) in priority order")
        (column integer "the column")
        (returns list))
  (define (cycle-sort keys column)
    ;; Ascending -> descending -> off. Direction retains priority; enabling
    ;; a new or previously disabled key appends it to the compound order.
    (let ([key (assv column keys)])
      (cond [(not key) (append keys (list (cons column #f)))]
            [(cdr key) (remq key keys)]
            [else (map (lambda (k) (if (eq? k key) (cons column #t) k)) keys)])))

  (edoc "A column's heading with its sort priority in superscript and direction arrow when it is a key."
        (table (record table) "the table")
        (keys list "the sort keys")
        (column integer "the column")
        (returns string))
  (define (heading table keys column)
    (string-append (vector-ref (table-headings table) column)
      (let loop ([keys keys] [priority 1])
        (cond [(null? keys) ""]
              [(= column (caar keys))
               (string-append
                 (list->string
                   (map (lambda (c) (string-ref "⁰¹²³⁴⁵⁶⁷⁸⁹" (- (char->integer c) 48)))
                     (string->list (number->string priority))))
                 (if (cdar keys) "↓" "↑"))]
              [else (loop (cdr keys) (+ priority 1))]))))

  (define (value<? a b)
    (cond [(not a) (and b #t)]
          [(not b) #f]
          [(boolean? a) #f]
          [(number? a) (< a b)]
          [else (string-ci<? a b)]))

  (edoc "Whether row a sorts before row b by the keys, the fallback deciding ties."
        (keys list "the sort keys")
        (value procedure "(value row column) giving a cell value")
        (fallback procedure "(fallback a b) for ties")
        (a any "one row")
        (b any "the other")
        (returns boolean))
  (define (less? keys value fallback a b)
    (let compare ([keys keys])
      (if (null? keys) (fallback a b)
          (let ([x (value a (caar keys))] [y (value b (caar keys))])
            (cond [(value<? x y) (not (cdar keys))]
                  [(value<? y x) (cdar keys)]
                  [else (compare (cdr keys))])))))

  (edoc "Fit a table into a width from its unfiltered rows: (values row columns), row a procedure formatting a row's data, columns the shown (column start end) spans."
        (table (record table) "the table")
        (keys list "the sort keys")
        (all (or list vector) "every row's data, or declared natural column widths")
        (cell procedure "(cell data column) giving a cell's text")
        (width integer "the columns available"))
  (define (layout table keys all cell width)
    ;; Size from the unfiltered rows so typing does not make columns jump.
    ;; Retain the identity column; drop unsorted metadata before sort keys.
    ;; Growth is bounded by the pane, not by the longest name on disk.
    (let* ([minimum (table-minimum table)] [sizes (vector-copy minimum)]
           [keep (table-keep table)] [indices (iota (vector-length minimum))]
           [columns
            (let fit ([columns indices]
                      [drop (append (filter (lambda (i) (not (assv i keys))) (table-drop table))
                              (remv keep (reverse (map car keys))))])
              (if (or (null? drop)
                      (<= (+ (* 2 (- (length columns) 1))
                             (apply + (map (lambda (i) (vector-ref minimum i)) columns))) width))
                  columns
                  (fit (remv (car drop) columns) (cdr drop))))]
           [natural
            (if (vector? all) all (list->vector
                                    (map (lambda (i)
                                           (fold-left (lambda (n row) (max n (glyph:cells (cell row i))))
                                             (vector-ref minimum i) all)) indices)))])
      (define (row data)
        (string:join
          (map (lambda (i)
                 (let* ([text (if data (cell data i) (heading table keys i))]
                        [size (vector-ref sizes i)]
                        [align (and data (vector-ref (table-alignment table) i))])
                   (if (eq? align 'right)
                       (let ([n (glyph:cells text)])
                         (if (> n size) (glyph:fit text size 'left)
                             (string-append (make-string (- size n) #\space) text)))
                       (glyph:fit text size (if (eq? align 'tail) 'left 'right)))))
            columns) "  "))
      (when (null? (cdr columns)) (vector-set! sizes keep width))
      (let grow ([room (- width (* 2 (- (length columns) 1))
                         (apply + (map (lambda (i) (vector-ref sizes i)) columns)))])
        (let ([want (filter (lambda (i) (< (vector-ref sizes i) (vector-ref natural i))) columns)])
          (when (and (> room 0) (pair? want))
            (let ([given (list-head want (min room (length want)))])
              (for-each (lambda (i) (vector-set! sizes i (+ 1 (vector-ref sizes i)))) given)
              (grow (- room (length given)))))))
      (values row
        (let bounds ([columns columns] [start 0])
          (if (null? columns) '()
              (let ([end (+ start (vector-ref sizes (car columns)))])
                (cons (list (car columns) start end) (bounds (cdr columns) (+ end 2)))))))))
  ;; One selection engine serves both presentations. Only the root owns
  ;; demand and navigation intent; rows are data, never individual views.
  (define sessions (make-hashtable equal-hash equal?))
  (define-record-type session
    (fields id query token (mutable generation) (mutable pending) (mutable ordinal)
      (mutable hovered) (mutable signature) (mutable neighbors) (mutable previous)))
  (define (get xs key fallback) (cond [(and xs (assq key xs)) => cdr] [else fallback]))
  (define (root id)
    (let ([d (interaction:snapshot id)])
      (cond [(not d) (error 'table "unavailable view" id)]
        [(memq (view:kind d) '(table list)) id]
        [(view:parent d) (root (view:parent d))] [else (error 'table "expected a table view" id)])))
  (define (runtime id) (or (hashtable-ref sessions (root id) #f) (error 'table "row source is unavailable" id)))
  (define (metadata s) (let ([r (range:summary (session-query s))]) (and r (eq? (get r 'kind #f) 'collection) (get r 'value #f))))
  (define (ready? v) (and v (eq? (get v 'status #f) 'ready)))
  (define (descriptor s) (interaction:snapshot (session-id s)))
  (define (selected s) (get (view:state (descriptor s)) 'selection #f))
  (define (hovered-row s)
    ;; Hover is a local candidate, not a selection publication. Keep its
    ;; shown generation, basis and ordinal so keys can adopt it coherently.
    (let ([h (session-hovered s)] [v (metadata s)])
      (and (pair? h) (eq? (car h) 'row) (ready? v)
        (equal? (car (cadr h)) (session-query s))
        (= (cadr (cadr h)) (get v 'generation 0)) h)))
  (define (columns s v)
    (let ([all (get v 'columns '())])
      (filter values (map (lambda (name) (assq name all)) (get (view:options (descriptor s)) 'columns (map car all))))))
  (define (child s name) (let ([p (assq name (view:children (descriptor s)))]) (and p (cadr p))))
  (define (body s)
    (let* ([scroll (child s 'body)] [d (and scroll (interaction:snapshot scroll))])
      (and d (pair? (view:children d)) (cadar (view:children d)))))
  (define (focused? s)
    (let loop ([id (widget:focused (session-id s))])
      (and id (or (equal? id (session-id s))
                (let ([d (interaction:snapshot id)]) (and d (loop (view:parent d))))))))
  (define (repaint! s)
    (for-each (lambda (id) (when id (widget:repaint! id #t))) (list (session-id s) (child s 'heading) (body s))))
  (define (release! id)
    (let ([s (hashtable-ref sessions id #f)])
      (when s (range:release! (session-token s)) (hashtable-delete! sessions id))))
  (define (find-frame f id)
    (and f (if (equal? id (widget:frame-id f)) f (exists (lambda (f) (find-frame f id)) (widget:frame-children f)))))
  (define (save-selection! s generation key basis ordinal)
    (let* ([selection (and ordinal (list (session-query s) generation key))]
           [state (list (cons 'selection selection) (cons 'basis basis))])
      (session-pending-set! s #f) (session-ordinal-set! s (or ordinal 0))
      (session-previous-set! s state)
      (when ordinal
        (session-neighbors-set! s
          (filter values
            (map (lambda (i)
                   (let ([r (range:read (session-query s) generation (max 0 i) 1 (map car (columns s (metadata s))))])
                     (and (eq? (car r) 'ready) (pair? (list-ref r 4))
                       (not (equal? key (cadar (list-ref r 4)))) (list (cadar (list-ref r 4)))))) (list (+ ordinal 1) (- ordinal 1))))))
      (unless (equal? state (view:state (descriptor s)))
        (interaction:set-state! head:ui-actor (session-id s) #f state) (repaint! s))))
  (define (mark-pending! s)
    ;; A connected consumer must not mistake the preceding row for a new,
    ;; unresolved choice. The last resolved state is only a cancellation aid.
    (when (selected s)
      (interaction:set-state! head:ui-actor (session-id s) #f '((selection . #f) (basis)))
      (repaint! s)))
  (define (seek! s intent v)
    (session-pending-set! s intent)
    (let* ([generation (get v 'generation 0)] [count (get v 'count 0)] [names (map car (columns s v))]
           [rank (and (eq? (car intent) 'key) (range:locate (session-query s) generation (cadr intent)))]
           [ordinal (cond [(zero? count) #f]
                      [(eq? (car intent) 'ordinal) (min (- count 1) (max 0 (cadr intent)))]
                      [(and rank (eq? (car rank) 'ready)) (list-ref rank 3)]
                      [else #f])])
      (cond [(zero? count) (save-selection! s generation #f '() #f)]
        [(and rank (eq? (car rank) 'ready) (not (list-ref rank 3)))
         (let ([neighbors (session-neighbors s)])
           (session-neighbors-set! s (if (pair? neighbors) (cdr neighbors) '()))
           (seek! s (if (pair? neighbors) (list 'key (caar neighbors)) '(ordinal 0)) v))]
        [ordinal
         (let ([page (range:read (session-query s) generation ordinal 1 names)])
           (if (and (eq? (car page) 'ready) (pair? (list-ref page 4)))
             (begin (save-selection! s generation (cadar (list-ref page 4)) (caddr page) ordinal)
               (widget:reveal! (body s) (list (session-query s) generation (cadar (list-ref page 4)))))
             (begin (mark-pending! s) (range:request! (session-token s) generation ordinal 1 names '()))))]
        [else (mark-pending! s) (range:request! (session-token s) generation 0 0 names (list (cadr intent)))])))
  (define (service! id frame)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (let* ([rows (assq 'rows inputs)] [query (and rows (eq? (cadr rows) 'ready) (caddr rows))]
             [old (hashtable-ref sessions id #f)])
        (when (and old (not (equal? query (session-query old)))) (release! id) (set! old #f))
        (when query
          (let* ([s (or old (let ([s (make-session id query (range:acquire! query (lambda ()
                                                                                    (let ([s (hashtable-ref sessions id #f)]) (when s (repaint! s))))) #f #f 0 #f #f '() (view:state d))])
                              (hashtable-set! sessions id s) s))]
                 [v (metadata s)] [g (get v 'generation 0)] [selection (selected s)])
            (unless (equal? (session-generation s) g)
              (session-generation-set! s g)
              (session-pending-set! s (if (and selection (equal? (car selection) query)) (list 'key (caddr selection)) '(ordinal 0))))
            (when (ready? v)
              (when (and (not (session-pending s)) (> (get v 'count 0) 0)
                      (or (not selection) (not (= (cadr selection) g))))
                (session-pending-set! s (if selection (list 'key (caddr selection)) '(ordinal 0))))
              (when (session-pending s) (seek! s (session-pending s) v))
              (unless (session-pending s)
                (let* ([f (find-frame frame (body s))]
                       [start (if f (max 0 (- (cadr (widget:frame-clip f)) (cadr (widget:frame-rect f)))) 0)]
                       [count (if f (min 256 (cadddr (widget:frame-clip f))) 1)]
                       [scroll (interaction:snapshot (child s 'body))]
                       [anchor (and scroll (view:state scroll))])
                  (range:request! (session-token s) g start count (map car (columns s v))
                    (if (and (row:selection? anchor) (equal? query (car anchor))
                          (not (eq? (car (range:locate query g (caddr anchor))) 'ready))) (list (caddr anchor)) '())))))
            (let ([signature (list v (view:state d) (view:options d) (session-pending s))])
              (unless (equal? signature (session-signature s)) (session-signature-set! s signature) (repaint! s))))))))

  (edoc "Create a table, or a single-column list, over a base collection. Each view keeps its own selection and geometry."
        (actor datum "creator") (query row-source "collection") (columns list "stable column names")
        (presentation (list-of symbol) "optional list, otherwise table") (returns list "root view"))
  (define (create! actor query columns . presentation)
    (unless (and (pair? columns) (for-all symbol? columns)
              (or (null? presentation) (equal? presentation '(list)))) (error 'create! "invalid table presentation"))
    (let* ([kind (if (null? presentation) 'table 'list)]
           [root (view:create! actor query kind 1 (list (cons 'columns (if (eq? kind 'list) (list (car columns)) columns))) '((selection . #f) (basis)))]
           [heading (and (eq? kind 'table) (view:create! actor #f 'table-heading 1 '() '()))]
           [scroll (view:create! actor #f 'scroll 1 '() #f)]
           [body (view:create! actor #f 'table-body 1 '() '())])
      (view:arrange! actor
        (list (list root 0 (append (if heading (list (list 'heading heading 'fit)) '()) (list (list 'body scroll '(grow 1)))) (list (cons 'columns (if heading columns (list (car columns))))))
          (list scroll 0 (list (list 'rows body '(grow 1))) '())) '()) root))

  (edoc "Select a stable row key, resolving its current rank asynchronously if needed." (id list "table or descendant") (key datum "row identity"))
  (define (select! id key)
    (let* ([s (runtime id)] [v (metadata s)])
      (session-hovered-set! s #f)
      (session-pending-set! s (list 'key key))
      (when (ready? v) (seek! s (session-pending s) v)) (repaint! s)))

  (edoc "Move from the hovered row or selection in result order, clearing hover. Uncached movement replaces bounded destination intent; activation waits for that row."
        (id list "table or descendant") (direction symbol "next, previous, first or last"))
  (define (move! id direction)
    (let* ([s (runtime id)] [v (metadata s)] [pending (session-pending s)] [hover (hovered-row s)]
           [at (if hover (cadddr hover)
                 (if (and pending (eq? (car pending) 'ordinal)) (cadr pending) (session-ordinal s)))]
           [count (get v 'count 0)]
           [next (case direction [(next) (+ at 1)] [(previous) (- at 1)] [(first) 0] [(last) (- count 1)] [else (error 'move! "invalid direction" direction)])])
      (session-hovered-set! s #f)
      (session-pending-set! s (list 'ordinal (max 0 (min (max 0 (- count 1)) next))))
      (when (ready? v) (seek! s (session-pending s) v)) (repaint! s)))

  (edoc "Adopt the hovered row, if any, and activate the selection through the explicit command binding; pending or stale rows refuse."
        (id list "table or descendant") (returns any))
  (define (activate! id)
    (let* ([s (runtime id)] [v (metadata s)] [hover (hovered-row s)])
      (when hover
        (session-hovered-set! s #f)
        (save-selection! s (cadr (cadr hover)) (caddr (cadr hover)) (caddr hover) (cadddr hover))
        (repaint! s))
      (let ([selection (selected s)])
        (unless (and (ready? v) selection (not (session-pending s))
                  (equal? (car selection) (session-query s)) (= (cadr selection) (get v 'generation 0)))
          (error 'activate! "selection is pending or unavailable"))
        (widget:invoke! (session-id s) 'activate selection (get (view:state (descriptor s)) 'basis '())))))

  (edoc "Set shared collection sorting using raw column values; selection remains local." (id list "table") (keys list "(column ascending-or-descending) entries"))
  (define (sort-by! id keys)
    (let* ([s (runtime id)] [r (range:summary (session-query s))])
      (let-values ([(status records) (collection:configure! head:ui-actor (session-query s) (get r 'revision #f) (list (cons 'sort keys)))])
        (unless (eq? status 'applied) (error 'sort-by! "collection changed; retry" status)))))

  (edoc "Cycle a stable column through ascending, descending and off, preserving compound priority." (id list "table") (column symbol "column name"))
  (define (toggle-sort! id column)
    (let* ([s (runtime id)] [v (metadata s)] [keys (get v 'sort '())] [key (assq column keys)])
      (sort-by! id (cond [(not key) (append keys (list (list column 'ascending)))]
                     [(eq? (cadr key) 'descending) (remq key keys)]
                     [else (map (lambda (k) (if (eq? key k) (list column 'descending) k)) keys)]))))

  (edoc "Choose visible columns in display order, retaining the first as the identity column." (id list "table") (names list "distinct column names"))
  (define (set-columns! id names)
    (let* ([s (runtime id)] [v (metadata s)] [id (session-id s)] [d (descriptor s)])
      (unless (and (pair? names) (list? names) (for-all (lambda (n) (assq n (get v 'columns '()))) names)
                (= (length names) (length (fold-left (lambda (out n) (if (memq n out) out (cons n out))) '() names)))
                (or (eq? (view:kind d) 'table) (= (length names) 1))) (error 'set-columns! "invalid column selection" names))
      (interaction:flush!)
      (widget:arrange! (list (list id (get (model:snapshot id) 'revision #f) (view:children d)
                               (cons (cons 'columns names) (remq (assq 'columns (view:options d)) (view:options d))))))))

  (define (data id source inputs) id)
  (define (cell row name)
    (let ([p (assq name (caddr row))])
      (cond [(not p) ""] [(eq? (cadr p) 'absent) ""] [(eq? (cadr p) 'ready) (if (string? (caddr p)) (caddr p) (format "~a" (caddr p)))] [else "[Unavailable]"])))
  (define (fit s v width)
    (let* ([cs (columns s v)] [n (length cs)] [names (map car cs)]
           [keys (filter values (map (lambda (k) (let ([tail (memq (car k) names)])
                                                   (and tail (cons (- n (length tail)) (eq? (cadr k) 'descending))))) (get v 'sort '())))]
           [t (make (list->vector (map cadr cs)) (list->vector (map (lambda (c) (if (eq? c (car cs)) 1 (max 4 (glyph:cells (cadr c))))) cs))
                0 (reverse (cdr (iota n))) (list->vector (map (lambda (c) (if (memq (caddr c) '(integer number)) 'right 'tail)) cs)))])
      (if (zero? n) (values (lambda (row) "") '())
        (layout t keys (list->vector (map (lambda (c) (if (eq? c (car cs)) width (max 12 (glyph:cells (cadr c))))) cs))
          (lambda (row i) (cell row (list-ref names i))) width))))
  ;; Frame data retains exact shown row keys and provenance for pointer hits.
  (define-record-type visible (fields session metadata rows status spans format selection hover focus))
  (define (viewport id d width height clip)
    (let* ([s (hashtable-ref sessions (root id) #f)] [v (and s (metadata s))]
           [heading? (eq? (view:kind d) 'table-heading)]
           [reply (and s (ready? v) (not heading?) (range:read (session-query s) (get v 'generation 0) (car clip) (min 256 (cdr clip)) (map car (columns s v))))]
           [selection (and s (selected s))])
      (let-values ([(format spans) (if (and s (pair? (columns s v))) (fit s v width) (values (lambda (row) "") '()))])
        (make-visible s v (and reply (eq? (car reply) 'ready) (list-ref reply 4)) (and reply (car reply)) spans format
          selection (and s (or (hovered-row s)
                             (let ([h (session-hovered s)]) (and (pair? h) (eq? (car h) 'column) h))))
          (and s (focused? s))))))
  (define (render v d width height clip)
    (cond [(eq? (view:kind d) 'table-heading) (list ((visible-format v) #f))]
      [(not (visible-rows v)) (list (glyph:fit (if (or (eq? (visible-status v) 'unavailable) (not (visible-metadata v)) (eq? (get (visible-metadata v) 'status #f) 'unavailable))
                                                   "[Unavailable rows]" "[Pending rows]") width))]
      [else (map (visible-format v) (visible-rows v))]))
  (define (measure id d axis cross child)
    (if (eq? axis 'x) '(1 1)
      (if (eq? (view:kind d) 'table-heading) '(1 1)
        (let* ([s (hashtable-ref sessions (root id) #f)] [v (and s (metadata s))]) (list 0 (get v 'count 1))))))
  (define (decorate v d width height clip)
    (cond [(eq? (view:kind d) 'table-heading)
           (cons (list (list 0 0 width 1) 'header)
             (if (and (pair? (visible-hover v)) (eq? (car (visible-hover v)) 'column))
               (let ([span (assv (cadr (visible-hover v)) (visible-spans v))])
                 (if span (list (list (list (cadr span) 0 (- (caddr span) (cadr span)) 1) '(header hover))) '())) '()))]
      [(not (visible-rows v)) (list (list (list 0 (car clip) width 1) 'ghost))]
      [else
       (filter values
         (map (lambda (row)
                (let ([selected (and (visible-selection v) (equal? (caddr (visible-selection v)) (cadr row)))]
                      [hover (and (pair? (visible-hover v)) (eq? (car (visible-hover v)) 'row) (cadr (visible-hover v)))])
                  (cond [(and hover (equal? (caddr hover) (cadr row))) (list (list 0 (car row) width 1) 'candidate-hover)]
                    [(and (not hover) selected (visible-focus v)) (list (list 0 (car row) width 1) 'candidate)] [else #f]))) (visible-rows v)))]))
  (define (anchor id ordinal width)
    (let* ([s (runtime id)] [v (metadata s)]
           [r (and (ready? v) (range:read (session-query s) (get v 'generation 0) ordinal 1 (map car (columns s v))))])
      (and r (eq? (car r) 'ready) (pair? (list-ref r 4)) (list (session-query s) (get v 'generation 0) (cadar (list-ref r 4))))))
  (define (locate id anchor width)
    (let* ([s (runtime id)] [v (metadata s)])
      (if (and (row:selection? anchor) (equal? (car anchor) (session-query s)))
        (let ([r (range:locate (session-query s) (get v 'generation 0) (caddr anchor))])
          (and (eq? (car r) 'ready) (or (list-ref r 3) 0))) 0)))
  (define (event! id source d event)
    (let ([s (runtime id)])
      (case (car event)
        [(blur cancel)
         (when (session-pending s)
           (let* ([state (session-previous s)] [selection (get state 'selection #f)] [v (metadata s)])
             (when (and selection (ready? v) (equal? (car selection) (session-query s)) (= (cadr selection) (get v 'generation 0)))
               (interaction:set-state! head:ui-actor (session-id s) #f state))))
         (session-hovered-set! s #f) (session-pending-set! s #f) (repaint! s)]
        [(pointer)
         (let* ([f (widget:event-frame)] [v (widget:frame-data f)] [phase (cadr event)]
                [heading? (eq? (view:kind d) 'table-heading)]
                [hit (and v (not (eq? phase 'leave))
                       (if heading?
                         (find (lambda (p) (<= (cadr p) (list-ref event 4) (- (caddr p) 1))) (visible-spans v))
                         (and (visible-rows v) (find (lambda (row) (= (car row) (list-ref event 5))) (visible-rows v)))))]
                [hover (and hit (if heading? (list 'column (car hit))
                                  (let ([shown (visible-metadata v)])
                                    (list 'row (list (session-query (visible-session v)) (get shown 'generation 0) (cadr hit))
                                      (get shown 'basis '()) (car hit)))))])
           (unless (equal? hover (session-hovered s)) (session-hovered-set! s hover) (repaint! s))
           (when (and hit (eq? phase 'press) (eq? (caddr event) 'primary))
             (if heading?
               (toggle-sort! (session-id s) (car (list-ref (columns s (visible-metadata v)) (car hit))))
               (let ([current (metadata s)] [shown (visible-metadata v)])
                 (when (and (ready? current) (equal? (session-query s) (session-query (visible-session v)))
                         (= (get current 'generation 0) (get shown 'generation -1)))
                   (save-selection! s (get shown 'generation 0) (cadr hit) (get shown 'basis '()) (car hit))
                   (session-hovered-set! s #f) (repaint! s)
                   (when (assq 'activate (widget:commands (session-id s))) (activate! (session-id s))))))))])))
  (define (sort-visible! id index)
    (let* ([s (runtime id)] [f (exists (lambda (p) (find-frame (car p) (child s 'heading))) (widget:shown))]
           [v (and f (widget:frame-data f))] [spans (if v (visible-spans v) '())])
      (when (< index (length spans))
        (toggle-sort! id (car (list-ref (columns s (visible-metadata v)) (car (list-ref spans index))))))))

  (edoc "Install list/table compositions, virtual rows, headings and their canonical commands.")
  (define (init!)
    (row:init!)
    (for-each (lambda (kind)
                (widget:register! kind 1
                  (append (layout:container 'y)
                    (list (cons 'service service!) (cons 'release release!) (cons 'contexts '(widget-table))
                      (cons 'actions (list (cons 'select select!) (cons 'move move!) (cons 'activate activate!)
                                       (cons 'sort-by sort-by!) (cons 'toggle-sort toggle-sort!) (cons 'set-columns set-columns!))))))) '(table list))
    (for-each (lambda (kind)
                (widget:register! kind 1
                  (append (list (cons 'prepare data) (cons 'viewport viewport) (cons 'render render) (cons 'measure measure) (cons 'decorate decorate) (cons 'event event!))
                    (if (eq? kind 'table-body) (list (cons 'focus #t) (cons 'anchor anchor) (cons 'locate locate)) '())))) '(table-heading table-body))
    (for-each (lambda (p) (keymap:bind-default! 'widget-table (car p) (keymap:call move! widget:target (cdr p))))
      '(("UP" . previous) ("DOWN" . next) ("HOME" . first) ("END" . last)))
    (keymap:bind-default! 'widget-table "RET" (keymap:call activate! widget:target))
    (for-each (lambda (i) (keymap:bind-default! 'widget-table (format "F~a" (+ i 1)) (keymap:call sort-visible! widget:target i))) (iota 12))))
