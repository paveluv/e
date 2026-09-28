;; Virtual collections and shared column rules. Legacy picker formatting
;; remains here until those apps adopt the composable controls.
(import (only (foundation edoc) elibrary))
(elibrary (head table)
  (export accept! activate! choose! create! cycle-sort emphasize! heading init! layout less? make move! register-presentation! select! set-columns! sort-by! target toggle-sort! toggle-visible-sort!)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (core row) row:) (prefix (foundation string) string:)
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
  (define presentations (kernel:make-registry car))
  (define presentation-changes
    (kernel:registry-observe! presentations
      (lambda (removed added) (vector-for-each repaint! (hashtable-values sessions)))))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (distinct? xs)
    (= (length xs) (length (fold-left (lambda (out x) (if (member x out) out (cons x out))) '() xs))))
  (define (roles? value) (or (symbol? value) (and (list? value) (for-all symbol? value))))
  (define (roles value) (if (symbol? value) (list value) value))

  (edoc "Register pure head column presentation. Each rule is (column minimum alignment dependencies formatter); dependencies names additional raw cells. Formatter receives the cell state, requested row cells and row attributes, returning (text (start end roles) ...), with character spans in the formatted text. No I/O or mutation belongs in a formatter."
        (name symbol "presentation name") (schema integer "positive version")
        (columns list "distinct column rules; alignment is text, tail or right"))
  (define (register-presentation! name schema columns)
    (unless (and (symbol? name) (natural? schema) (> schema 0) (list? columns)
              (for-all (lambda (c) (and (list? c) (= (length c) 5) (symbol? (car c))
                                     (natural? (cadr c)) (memq (caddr c) '(text tail right))
                                     (list? (cadddr c)) (for-all symbol? (cadddr c)) (procedure? (list-ref c 4)))) columns)
              (distinct? (map car columns))) (error 'register-presentation! "invalid column presentation"))
    (kernel:registry-add! presentations (cons (list name schema) columns)))
  (define-record-type session
    (fields id query token (mutable generation) (mutable pending) (mutable ordinal)
      (mutable hovered) (mutable signature) (mutable neighbors) (mutable previous) (mutable display) (mutable fitting)))
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
  (define (requested-columns s v)
    (let* ([names (map car (columns s v))]
           [profile (get (view:options (descriptor s)) 'presentation #f)]
           [definition (and profile (kernel:registry-find presentations (lambda (p) (equal? profile (car p)))))])
      (fold-left (lambda (out name) (if (memq name out) out (cons name out))) names
        (if definition (apply append (map (lambda (c) (if (memq (car c) names) (cadddr c) '())) (cdr definition))) '()))))
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
    (hashtable-delete! emphasis id)
    (let ([s (hashtable-ref sessions id #f)])
      (when s (range:release! (session-token s)) (hashtable-delete! sessions id))))
  (define (find-frame f id)
    (and f (if (equal? id (widget:frame-id f)) f (exists (lambda (f) (find-frame f id)) (widget:frame-children f)))))
  (define (viewport-demand s v)
    (let* ([f (exists (lambda (p) (find-frame (car p) (body s))) (widget:shown))]
           [count (if f (min 256 (cadddr (widget:frame-clip f))) 1)]
           [start (if f (- (cadr (widget:frame-clip f)) (cadr (widget:frame-rect f))) 0)])
      (values (max 0 (min start (- (get v 'count 0) count))) count)))
  (define (display-metadata s)
    (let ([display (and s (session-display s))]) (if display (car display) (and s (metadata s)))))
  (define (refresh-display! s v)
    ;; Keep one bounded viewport until its replacement is complete. It is
    ;; presentation only: selection and actions always validate current data.
    (let ([old (session-display s)])
      (cond [(or (not v) (eq? (get v 'status #f) 'unavailable)) (session-display-set! s #f)]
        [(ready? v)
         (let-values ([(start count) (viewport-demand s v)])
           (let ([r (range:read (session-query s) (get v 'generation 0) start count (requested-columns s v))])
             (case (car r)
               [(ready) (session-display-set! s (list v start (list-ref r 4) (selected s)))]
               [(unavailable)
                (session-display-set! s (list (cons '(status . unavailable) (filter (lambda (p) (not (eq? (car p) 'status))) v)) start #f #f))])))])
      (unless (equal? old (session-display s)) (repaint! s))))
  (define (save-selection! s generation key basis ordinal)
    (let* ([selection (and ordinal (list (session-query s) generation key))]
           [state (list (cons 'selection selection) (cons 'basis basis))])
      (session-pending-set! s #f) (session-ordinal-set! s (or ordinal 0))
      (session-previous-set! s state)
      (when ordinal
        (session-neighbors-set! s
          (filter values
            (map (lambda (direction)
                   (let* ([destination (range:seek (session-query s) generation ordinal direction 1)]
                          [i (and (eq? (car destination) 'ready) (list-ref destination 3))]
                          [r (and i (range:read (session-query s) generation i 1 (requested-columns s (metadata s))))])
                     (and r (eq? (car r) 'ready) (pair? (list-ref r 4))
                       (not (equal? key (cadar (list-ref r 4)))) (list (cadar (list-ref r 4)))))) '(forward backward)))))
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
    (let* ([generation (get v 'generation 0)] [count (get v 'count 0)] [names (requested-columns s v)]
           [rank (and (eq? (car intent) 'key) (range:locate (session-query s) generation (cadr intent)))]
           [navigation (and (eq? (car intent) 'ordinal)
                         (list (min (max 0 (- count 1)) (max 0 (cadr intent)))
                           (if (pair? (cddr intent)) (caddr intent) 'forward)
                           (if (and (pair? (cddr intent)) (pair? (cdddr intent))) (cadddr intent) 0)))]
           [seek (and navigation (apply range:seek (session-query s) generation navigation))]
           [ordinal (cond [(zero? count) #f]
                      [(and seek (eq? (car seek) 'ready)) (list-ref seek 3)]
                      [(and rank (eq? (car rank) 'ready)) (list-ref rank 3)]
                      [else #f])])
      (cond [(zero? count) (save-selection! s generation #f '() #f)]
        [(and seek (eq? (car seek) 'ready) (not (list-ref seek 3)))
         (save-selection! s generation #f (caddr seek) #f)]
        [(and rank (eq? (car rank) 'ready) (not (list-ref rank 3)))
         (let ([neighbors (session-neighbors s)])
           (session-neighbors-set! s (if (pair? neighbors) (cdr neighbors) '()))
           (seek! s (if (pair? neighbors) (list 'key (caar neighbors)) '(ordinal 0)) v))]
        [ordinal
         (let ([page (range:read (session-query s) generation ordinal 1 names)])
           (if (and (eq? (car page) 'ready) (pair? (list-ref page 4)))
             (let ([row (car (list-ref page 4))])
               (if (get (cadddr row) 'selectable #t)
                 (begin (save-selection! s generation (cadr row) (caddr page) ordinal)
                   (widget:reveal! (body s) (list (session-query s) generation (cadr row))))
                 (if navigation (error 'table "provider selected an ineligible row" (cadr row)) (seek! s '(ordinal 0) v))))
             (begin (mark-pending! s) (range:request! (session-token s) generation ordinal 1 names '()))))]
        [else (mark-pending! s)
          (let-values ([(start count) (viewport-demand s v)])
            (range:request! (session-token s) generation start count names
              (if navigation '() (list (cadr intent))) (if navigation (list navigation) '())))])))
  (define (service! id frame)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (let* ([rows (assq 'rows inputs)] [query (and rows (eq? (cadr rows) 'ready) (caddr rows))]
             [old (hashtable-ref sessions id #f)])
        (when (and old (not (equal? query (session-query old)))) (release! id) (set! old #f))
        (when query
          (let* ([s (or old (let ([s (make-session id query (range:acquire! query (lambda ()
                                                                                    (let ([s (hashtable-ref sessions id #f)]) (when s (repaint! s))))) #f #f 0 #f #f '() (view:state d) #f #f)])
                              (hashtable-set! sessions id s) s))]
                 [v (metadata s)] [g (get v 'generation 0)] [selection (selected s)])
            (when (and (ready? v) (not (equal? (session-generation s) g)))
              (session-generation-set! s g)
              (unless (session-pending s)
                (let ([choice (or selection (get (session-previous s) 'selection #f))])
                  (session-pending-set! s
                    (if (and choice (equal? (car choice) query)) (list 'key (caddr choice))
                      (if (pair? (get v 'default '())) (list 'key (car (get v 'default '()))) '(ordinal 0)))))))
            (unless (ready? v) (mark-pending! s))
            (when (ready? v)
              (when (and (not (session-pending s)) (> (get v 'count 0) 0)
                      (or (not selection) (not (= (cadr selection) g))))
                (session-pending-set! s (if selection (list 'key (caddr selection)) '(ordinal 0))))
              (when (session-pending s) (seek! s (session-pending s) v))
              (unless (session-pending s)
                (let-values ([(start count) (viewport-demand s v)])
                  (let* ([scroll (interaction:snapshot (child s 'body))]
                         [anchor (and scroll (view:state scroll))])
                    (range:request! (session-token s) g start count (requested-columns s v)
                      (fold-left (lambda (keys ref)
                                   (if (and (row:selection? ref) (equal? query (car ref))
                                         (not (member (caddr ref) keys))
                                         (not (eq? (car (range:locate query g (caddr ref))) 'ready)))
                                     (cons (caddr ref) keys) keys)) '() (list anchor (selected s))))))))
            (refresh-display! s v)
            (let ([signature (list v (view:state d) (view:options d) (session-pending s))])
              (unless (equal? signature (session-signature s)) (session-signature-set! s signature) (repaint! s))))))))

  (edoc "Create a table, or a single-column list, over a base collection. Each view keeps its own selection and geometry."
        (actor datum "creator") (query row-source "collection") (columns list "stable column names")
        (configuration (list-of list) "optional alist: kind table|list, identity column (defaults to first), presentation (name schema)") (returns model "root view"))
  (define (create! actor query columns . configuration)
    (unless (and (list? columns) (pair? columns) (for-all symbol? columns) (distinct? columns)
              (<= (length configuration) 1)) (error 'create! "invalid table columns/options"))
    (let* ([config (if (null? configuration) '() (car configuration))]
           [valid (and (list? config) (for-all (lambda (p) (and (pair? p) (memq (car p) '(kind identity presentation)))) config)
                    (distinct? (map car config)))]
           [kind (and valid (get config 'kind 'table))] [identity (and valid (get config 'identity (car columns)))]
           [profile (and valid (get config 'presentation #f))])
      (unless (and (memq kind '(table list)) (memq identity columns)
                (or (eq? kind 'table) (= (length columns) 1))
                (or (not profile) (and (list? profile) (= (length profile) 2) (symbol? (car profile)) (natural? (cadr profile)) (> (cadr profile) 0))))
        (error 'create! "invalid table presentation" config))
      (let* ([options (append (list (cons 'columns columns) (cons 'identity identity)) (if profile (list (cons 'presentation profile)) '()))]
             [root (view:create! actor query kind 1 options '((selection . #f) (basis)))]
             [heading (and (eq? kind 'table) (view:create! actor #f 'table-heading 1 '() '()))]
             [scroll (view:create! actor #f 'scroll 1 '() #f)]
             [body (view:create! actor #f 'table-body 1 '() '())])
        (view:arrange! actor
          (list (list root 0 (append (if heading (list (list 'heading heading 'fit)) '()) (list (list 'body scroll '(grow 1)))) options)
            (list scroll 0 (list (list 'rows body '(grow 1))) '())) '()) root)))

  (edoc "Select a stable row key, resolving its current rank asynchronously if needed." (id model "table or descendant") (key datum "row identity"))
  (define (select! id key)
    (let* ([s (runtime id)]
           ;; Explicit selection is an action boundary. A filter edit may
           ;; have invalidated the base before its mirror notification lands.
           [barrier (model:snapshots (list (session-query s)))] [v (metadata s)])
      (session-hovered-set! s #f)
      (session-pending-set! s (list 'key key 'explicit))
      (when (ready? v) (seek! s (session-pending s) v)) (repaint! s)))

  (define (page-size s)
    (let ([f (exists (lambda (p) (find-frame (car p) (body s))) (widget:shown))])
      (if f (max 1 (cadddr (widget:frame-clip f))) 1)))

  (edoc "Move among selectable rows, clearing hover. Page motion uses the shown viewport height. Repeated uncached motion replaces bounded intent; activation waits for the destination."
        (id model "table or descendant") (direction (one-of next previous first last page-next page-previous) "row or page motion"))
  (define (move! id direction)
    (let* ([s (runtime id)] [v (metadata s)] [pending (session-pending s)] [hover (hovered-row s)]
           [at (if hover (cadddr hover)
                 (if (and pending (eq? (car pending) 'ordinal)) (cadr pending) (session-ordinal s)))]
           [count (get v 'count 0)]
           [steps (if (and (not hover) pending (eq? (car pending) 'ordinal) (= (length pending) 4))
                    (* (cadddr pending) (if (eq? (caddr pending) 'forward) 1 -1)) 0)]
           [intent (case direction
                     [(first) '(ordinal 0 forward 0)] [(last) (list 'ordinal (max 0 (- count 1)) 'backward 0)]
                     [(page-next page-previous)
                      (let ([forward? (eq? direction 'page-next)])
                        (list 'ordinal (max 0 (min (max 0 (- count 1)) (+ at (* (if forward? 1 -1) (page-size s)))))
                          (if forward? 'forward 'backward) 0))]
                     [(next previous)
                      (let ([delta (+ steps (if (eq? direction 'next) 1 -1))])
                        (list 'ordinal at (if (negative? delta) 'backward 'forward) (min count (abs delta))))]
                     [else (error 'move! "invalid direction" direction)])])
      (session-hovered-set! s #f)
      (session-pending-set! s intent)
      (when (ready? v) (seek! s (session-pending s) v)) (repaint! s)))

  (edoc "Read the hovered or selected target from local cached rows without adopting it or scheduling work. Return (selection basis row), where row is (ordinal key cells attributes), or false while selection or row data is unavailable."
        (id model "table or descendant") (returns (or list #f)) (effects internal))
  (define (target id)
    (let* ([s (hashtable-ref sessions (root id) #f)])
      (and s
        (let* ([v (metadata s)] [hover (hovered-row s)]
               [selection (if hover (cadr hover) (and (not (session-pending s)) (selected s)))]
               [ordinal (if hover (cadddr hover) (session-ordinal s))]
               [r (and (ready? v) selection (equal? (car selection) (session-query s))
                    (= (cadr selection) (get v 'generation -1))
                    (range:read (session-query s) (cadr selection) ordinal 1 (requested-columns s v)))])
          (and r (eq? (car r) 'ready) (pair? (list-ref r 4))
            (let ([row (car (list-ref r 4))])
              (and (equal? (caddr selection) (cadr row)) (get (cadddr row) 'selectable #t)
                (list selection (caddr r) row))))))))

  (edoc "Adopt an inspected target only while it remains the current hover or selection. This is an execution boundary, never a discovery operation; the command must separately validate its authoritative mutation."
        (id model "table or descendant") (chosen list "snapshot returned by table:target"))
  (define (accept! id chosen)
    (let ([current (target id)] [s (runtime id)])
      (unless (and current (equal? current chosen)) (error 'accept! "the selected result changed"))
      (session-hovered-set! s #f)
      (save-selection! s (cadar chosen) (caddar chosen) (cadr chosen) (car (caddr chosen)))
      (repaint! s)))

  (edoc "Adopt the hovered row, if any, and activate the selection through the explicit command binding; pending or stale rows refuse."
        (id model "table or descendant") (command (list-of symbol) "optional command binding, default activate") (returns any))
  (define (activate! id . command)
    (unless (or (null? command) (and (= (length command) 1) (symbol? (car command))))
      (error 'activate! "expected an optional command name"))
    (let* ([s (runtime id)] [intent (session-pending s)]
           [explicit? (and intent (eq? (car intent) 'key) (= (length intent) 3)
                        (or (null? command) (eq? (car command) 'activate)))]
           [barrier (and explicit? (model:snapshots (list (session-query s))))]
           [v (metadata s)] [hover (hovered-row s)])
      ;; A host can choose a known identity and immediately accept it before
      ;; its page arrives. Resolve that exact intent once, never a neighboring
      ;; row or a stale ordinal. Preparing providers still refuse promptly.
      (when (and explicit? (ready? v))
        (let ([r (collection:lookup (session-query s) (get v 'generation 0) (cadr intent) '())])
          (when (and (eq? (car r) 'ready) (pair? (list-ref r 4)) (get (cadddr (car (list-ref r 4))) 'selectable #t))
            (save-selection! s (cadr r) (cadr intent) (caddr r) (caar (list-ref r 4)))
            (widget:reveal! (body s) (list (session-query s) (cadr r) (cadr intent))))))
      (when hover
        (session-hovered-set! s #f)
        (save-selection! s (cadr (cadr hover)) (caddr (cadr hover)) (caddr hover) (cadddr hover))
        (repaint! s))
      (let ([selection (selected s)])
        (unless (and (ready? v) selection (not (session-pending s))
                  (equal? (car selection) (session-query s)) (= (cadr selection) (get v 'generation 0)))
          (error 'activate! "selection is pending or unavailable"))
        (widget:invoke! (session-id s) (if (null? command) 'activate (car command)) selection (get (view:state (descriptor s)) 'basis '())))))

  (edoc "Set shared collection sorting using raw column values; selection remains local." (id model "table") (keys list "(column ascending-or-descending) entries"))
  (define (sort-by! id keys)
    (let* ([s (runtime id)] [r (range:summary (session-query s))])
      (let-values ([(status records) (collection:configure! head:ui-actor (session-query s) (get r 'revision #f) (list (cons 'sort keys)))])
        (unless (eq? status 'applied) (error 'sort-by! "collection changed; retry" status)))))

  (edoc "Cycle a stable column through ascending, descending and off, preserving compound priority." (id model "table") (column symbol "column name"))
  (define (toggle-sort! id column)
    (let* ([s (runtime id)] [v (metadata s)] [keys (get v 'sort '())] [key (assq column keys)])
      (sort-by! id (cond [(not key) (append keys (list (list column 'ascending)))]
                     [(eq? (cadr key) 'descending) (remq key keys)]
                     [else (map (lambda (k) (if (eq? key k) (list column 'descending) k)) keys)]))))

  (edoc "Choose visible columns in display order; the declared identity column must remain present." (id model "table") (names list "distinct column names"))
  (define (set-columns! id names)
    (let* ([s (runtime id)] [v (metadata s)] [id (session-id s)] [d (descriptor s)])
      (unless (and (pair? names) (list? names) (for-all (lambda (n) (assq n (get v 'columns '()))) names)
                (= (length names) (length (fold-left (lambda (out n) (if (memq n out) out (cons n out))) '() names)))
                (memq (get (view:options d) 'identity (car names)) names)
                (or (eq? (view:kind d) 'table) (= (length names) 1))) (error 'set-columns! "invalid column selection" names))
      (interaction:flush!)
      (widget:arrange! (list (list id (get (model:snapshot id) 'revision #f) (view:children d)
                               (cons (cons 'columns names) (remq (assq 'columns (view:options d)) (view:options d))))))))

  (define (data id source inputs) id)
  (define (cell row name rule identity width)
    (let* ([p (assq name (caddr row))] [attributes (cadddr row)]
           [display
            (cond [rule ((list-ref rule 4) (if p (cdr p) '(absent)) (caddr row) attributes)]
              [(or (not p) (eq? (cadr p) 'absent)) '("")]
              [(not (eq? (cadr p) 'ready))
               (let ([text (if (eq? (cadr p) 'pending) "[Pending]" "[Unavailable]")]) (list text (list 0 (string-length text) 'ghost)))]
              [else
               (let ([text (if (string? (caddr p)) (caddr p) (format "~a" (caddr p)))])
                 (cons text (filter values
                              (map (lambda (span) (and (eq? (car span) name) (string? (caddr p))
                                                    (list (cadr span) (caddr span) 'mark))) (get attributes 'matches '())))))])])
      (unless (and (list? display) (pair? display) (string? (car display))
                (for-all (lambda (span) (and (list? span) (= (length span) 3) (natural? (car span))
                                          (natural? (cadr span)) (< (car span) (cadr span)) (<= (cadr span) (string-length (car display))) (roles? (caddr span)))) (cdr display)))
        (error 'table "invalid cell presentation" name))
      (if (not (eq? name identity)) display
        (let* ([indent (min width (get attributes 'depth 0))] [text (car display)]
               [end (+ indent (string-length text))] [creation (get attributes 'creation #f)])
          (cons (string-append (make-string indent #\space) text (if creation " [create]" ""))
            (append (map (lambda (span) (list (+ indent (car span)) (+ indent (cadr span)) (caddr span))) (cdr display))
              (if creation (append (if (> end indent) (list (list indent end 'italic)) '())
                             (list (list end (+ end 9) 'ghost))) '())))))))
  ;; Map formatted character spans through the same whole-cluster clipping
  ;; used by glyph:fit. Neither padding nor the ellipsis inherits a match.
  (define (cell-styles display size align base)
    (if (null? (cdr display)) '()
      (let* ([text (car display)] [parts (glyph:clusters text)] [length (string-length text)]
             [total (apply + (map cdr parts))] [left? (memq align '(tail right))]
             [kept (if (<= total size) parts
                     (let loop ([ps (if left? (reverse parts) parts)] [room (max 0 (- size 1))] [out '()])
                       (if (or (null? ps) (> (cdar ps) room))
                         (if left? out (reverse out)) (loop (cdr ps) (- room (cdar ps)) (cons (car ps) out)))))]
             [char (if (and left? (> total size)) (- length (apply + (map car kept))) 0)]
             [start (cond [(and (> total size) left?) 1] [(and (eq? align 'right) (<= total size)) (- size total)] [else 0])])
        (let loop ([ps kept] [char char] [at start] [out '()])
          (if (null? ps) (reverse out)
            (let* ([next (+ char (caar ps))]
                   [rs (fold-left (lambda (out span)
                                    (if (and (< (car span) next) (> (cadr span) char))
                                      (append out (roles (caddr span))) out)) base (cdr display))])
              (loop (cdr ps) next (+ at (cdar ps))
                (if (equal? rs base) out
                  (if (and (pair? out) (equal? rs (caddar out)) (= at (+ (caar out) (cadar out))))
                    (cons (list (caar out) (+ (cadar out) (cdar ps)) rs) (cdr out))
                    (cons (list at (cdar ps) rs) out))))))))))
  (define (fit s v width)
    (let* ([cs (columns s v)] [n (length cs)] [names (map car cs)]
           [options (view:options (descriptor s))] [identity (get options 'identity (and (pair? names) (car names)))]
           [keep (let ([tail (memq identity names)]) (if tail (- n (length tail)) 0))]
           [profile (get options 'presentation #f)]
           [definition (and profile (kernel:registry-find presentations (lambda (p) (equal? profile (car p)))))]
           [rules (if definition (cdr definition) '())] [cache (make-eq-hashtable)]
           [keys (filter values (map (lambda (k) (let ([tail (memq (car k) names)])
                                                   (and tail (cons (- n (length tail)) (eq? (cadr k) 'descending))))) (get v 'sort '())))]
           [alignment (list->vector (map (lambda (c) (cond [(assq (car c) rules) => caddr]
                                                       [(memq (caddr c) '(integer number)) 'right] [(eq? (car c) identity) 'text] [else 'tail])) cs))]
           [t (make (list->vector (map cadr cs))
                (list->vector (map (lambda (c) (cond [(eq? (car c) identity) 1] [(assq (car c) rules) => cadr] [else (max 4 (glyph:cells (cadr c)))])) cs))
                keep (reverse (remv keep (iota n))) alignment)])
      (define (present row i)
        (let ([cells (or (hashtable-ref cache row #f) (let ([cells (make-vector n #f)]) (hashtable-set! cache row cells) cells))])
          (or (vector-ref cells i)
            (let ([display (cell row (list-ref names i) (assq (list-ref names i) rules) identity width)])
              (vector-set! cells i display) display))))
      (define (styles row span base)
        (let ([i (car span)]) (cell-styles (present row i) (- (caddr span) (cadr span)) (vector-ref alignment i) base)))
      (when (and profile (not definition)) (error 'table "column presentation is unavailable" profile))
      (if (zero? n) (values (lambda (row) "") '() styles)
        (let* ([basis (list definition cs)] [old (session-fitting s)]
               [natural (if (and old (equal? (car old) basis)) (vector-copy (cadr old)) (make-vector n 0))]
               [display (session-display s)] [rows (if display (or (caddr display) '()) '())])
          ;; Measure only the retained viewport. Remember observed maxima so
          ;; filtering and scrolling do not repeatedly squeeze the columns.
          ;; These are head-local measurements, never provider/wire widths.
          (for-each
            (lambda (i)
              (vector-set! natural i
                (fold-left (lambda (size row) (max size (glyph:cells (car (present row i)))))
                  (max (vector-ref natural i) (vector-ref (table-minimum t) i) (glyph:cells (heading t keys i))) rows)))
            (iota n))
          (session-fitting-set! s (list basis (vector-copy natural)))
          ;; The last column receives spare room; identity only asks for the
          ;; space its names need instead of crowding out file paths.
          (vector-set! natural (- n 1) (max width (vector-ref natural (- n 1))))
          (let-values ([(format spans) (layout t keys natural (lambda (row i) (car (present row i))) width)])
            (values format spans styles))))))
  ;; Frame data retains exact shown row keys and provenance for pointer hits.
  (define emphasis (make-hashtable equal-hash equal?))

  (edoc "Emphasize a host's current document key without changing table selection or publishing interaction state. False clears the emphasis."
        (id model "table view") (key datum "row identity or false"))
  (define (emphasize! id key)
    (let ([id (root id)])
      (unless (equal? key (hashtable-ref emphasis id #f))
        (if key (hashtable-set! emphasis id key) (hashtable-delete! emphasis id))
        (let ([s (hashtable-ref sessions id #f)]) (when s (repaint! s))))))
  (define-record-type visible (fields session metadata rows status spans format styles selection hover focus emphasis))
  (define (viewport id d width height clip)
    (let* ([s (hashtable-ref sessions (root id) #f)] [current (and s (metadata s))]
           [display (and s (session-display s))] [v (if display (car display) current)]
           [pending? (or (not (ready? current)) (not display) (not (= (get v 'generation -1) (get current 'generation 0))))]
           [rows (and display (list? (caddr display)) (<= (cadr display) (car clip))
                   (<= (min (get v 'count 0) (+ (car clip) (min 256 (cdr clip)))) (+ (cadr display) (length (caddr display))))
                   (filter (lambda (r) (<= (car clip) (car r) (- (+ (car clip) (cdr clip)) 1))) (caddr display)))]
           [selection (and s (if (and pending? display) (cadddr display) (selected s)))])
      (let-values ([(format spans styles) (if (and s (pair? (columns s v))) (fit s v width) (values (lambda (row) "") '() (lambda args '())))])
        (make-visible s v rows (if pending? 'pending 'ready) spans format styles
          selection (and s (or (hovered-row s)
                             (let ([h (session-hovered s)]) (and (pair? h) (eq? (car h) 'column) h))))
          (and s (focused? s)) (and s (hashtable-ref emphasis (session-id s) #f))))))
  (define (busy? id d)
    (let* ([s (hashtable-ref sessions id #f)] [current (and s (metadata s))]
           [display (and s (session-display s))])
      (and s (not (eq? (get current 'status #f) 'unavailable))
        (or (not (ready? current)) (not display)
          (not (= (get (car display) 'generation -1) (get current 'generation 0)))
          (and (session-pending s) #t)))))
  (define (render v d width height clip)
    (cond [(eq? (view:kind d) 'table-heading)
           (list ((visible-format v) #f))]
      [(not (visible-rows v)) (list (glyph:fit (if (or (eq? (visible-status v) 'unavailable) (not (visible-metadata v)) (eq? (get (visible-metadata v) 'status #f) 'unavailable))
                                                   "[Unavailable rows]" "[Pending rows]") width))]
      [(null? (visible-rows v))
       (list (glyph:fit (get (view:options (descriptor (visible-session v))) 'empty-text "No matches") width))]
      [else (map (visible-format v) (visible-rows v))]))
  (define (measure id d axis cross child)
    (if (eq? axis 'x) '(1 1)
      (if (eq? (view:kind d) 'table-heading) '(1 1)
        (let* ([s (hashtable-ref sessions (root id) #f)] [v (display-metadata s)]) (list 0 (max 1 (get v 'count 1)))))))
  (define (decorate v d width height clip)
    (cond [(eq? (view:kind d) 'table-heading)
           (append (list (list (list 0 0 width 1) 'header))
             (if (and (pair? (visible-hover v)) (eq? (car (visible-hover v)) 'column))
               (let ([span (assv (cadr (visible-hover v)) (visible-spans v))])
                 (if span (list (list (list (cadr span) 0 (- (caddr span) (cadr span)) 1) '(header hover))) '())) '()))]
      [(or (not (visible-rows v)) (null? (visible-rows v))) (list (list (list 0 (car clip) width 1) 'ghost))]
      [else
       (apply append
         (map (lambda (row)
                (let* ([selected (and (visible-selection v) (equal? (caddr (visible-selection v)) (cadr row)))]
                       [hover (and (pair? (visible-hover v)) (eq? (car (visible-hover v)) 'row) (cadr (visible-hover v)))]
                       [base (append (get (cadddr row) 'roles '()) (if (get (cadddr row) 'selectable #t) '() '(header))
                               (cond [(and hover (equal? (caddr hover) (cadr row))) '(candidate-hover)]
                                 [(and (not hover) selected (visible-focus v)) '(candidate)]
                                 [(equal? (visible-emphasis v) (cadr row)) '(active)] [else '()]))])
                  (append (if (null? base) '() (list (list (list 0 (car row) width 1) (if (null? (cdr base)) (car base) base))))
                    (apply append (map (lambda (span)
                                         (map (lambda (style) (list (list (+ (cadr span) (car style)) (car row) (cadr style) 1) (caddr style)))
                                           ((visible-styles v) row span base))) (visible-spans v)))))) (visible-rows v)))]))
  (define (anchor id ordinal width)
    (let* ([s (runtime id)] [v (metadata s)]
           [r (and (ready? v) (range:read (session-query s) (get v 'generation 0) ordinal 1 (requested-columns s v)))])
      (and r (eq? (car r) 'ready) (pair? (list-ref r 4)) (list (session-query s) (get v 'generation 0) (cadar (list-ref r 4))))))
  (define (locate id anchor width)
    (let* ([s (runtime id)] [v (metadata s)])
      (if (and (row:selection? anchor) (equal? (car anchor) (session-query s)))
        (let ([r (range:locate (session-query s) (get v 'generation 0) (caddr anchor))])
          (and (eq? (car r) 'ready) (or (list-ref r 3) 0))) 0)))

  (edoc "Choose an explicitly identified displayed row and activate it if the table has an activate command. Refuse a stale result or a row no longer displayed; no deferred activation is queued."
        (id model "table or descendant") (selection list "(collection generation key)"))
  (define (choose! id selection)
    (let* ([s (runtime id)] [current (metadata s)] [display (session-display s)]
           [row (and (row:selection? selection) display (ready? current)
                  (equal? (car selection) (session-query s)) (= (cadr selection) (get current 'generation -1))
                  (= (cadr selection) (get (car display) 'generation -1))
                  (find (lambda (row) (and (equal? (cadr row) (caddr selection)) (get (cadddr row) 'selectable #t))) (or (caddr display) '())))])
      (unless row (error 'choose! "displayed row is no longer available" selection))
      (save-selection! s (cadr selection) (caddr selection) (get current 'basis '()) (car row))
      (session-hovered-set! s #f) (repaint! s)
      (when (assq 'activate (widget:commands (session-id s))) (activate! (session-id s)))))

  (define (pointer-hit f x y)
    (let* ([v (widget:frame-data f)] [s (runtime (widget:frame-id f))]
           [heading? (eq? (view:kind (widget:frame-descriptor f)) 'table-heading)])
      (and v
        (if heading?
          (find (lambda (p) (and (memq (car (list-ref (columns s (visible-metadata v)) (car p))) (get (visible-metadata v) 'sortable '()))
                              (<= (cadr p) x (- (caddr p) 1)))) (visible-spans v))
          (and (visible-rows v) (find (lambda (row) (and (get (cadddr row) 'selectable #t) (= (car row) y))) (visible-rows v)))))))
  (define (pointer-bindings f x y)
    (let* ([s (runtime (widget:frame-id f))] [v (widget:frame-data f)] [hit (pointer-hit f x y)]
           [current (metadata s)] [shown (and v (visible-metadata v))])
      (if (not (and hit (ready? current) (equal? (session-query s) (session-query (visible-session v)))
                    (= (get current 'generation 0) (get shown 'generation -1)))) '()
        (list (list '(click primary ())
                (if (eq? (view:kind (widget:frame-descriptor f)) 'table-heading)
                  (keymap:call toggle-sort! (session-id s) (car (list-ref (columns s shown) (car hit))))
                  (keymap:call choose! (session-id s) (list (session-query s) (get shown 'generation 0) (cadr hit)))))))))
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
                [hit (and (not (eq? phase 'leave)) (pointer-hit f (list-ref event 4) (list-ref event 5)))]
                [hover (and hit (if heading? (list 'column (car hit))
                                  (let ([shown (visible-metadata v)])
                                    (list 'row (list (session-query (visible-session v)) (get shown 'generation 0) (cadr hit))
                                      (get shown 'basis '()) (car hit)))))])
           (unless (equal? hover (session-hovered s)) (session-hovered-set! s hover) (repaint! s))
           (or (memq phase '(move leave))
             (and hit (eq? phase 'press) (eq? (caddr event) 'primary)
               (let ([binding (assoc '(click primary ()) (pointer-bindings f (list-ref event 4) (list-ref event 5)))])
                 (and binding (begin (keymap:run! (cadr binding)) #t))))))])))

  (edoc "Cycle sorting for a currently visible heading by position. An absent or unsortable heading does nothing."
        (id model "table or descendant") (index integer "zero-based visible column position"))
  (define (toggle-visible-sort! id index)
    (unless (natural? index) (error 'toggle-visible-sort! "expected a nonnegative column index" index))
    (let* ([s (runtime id)] [f (exists (lambda (p) (find-frame (car p) (child s 'heading))) (widget:shown))]
           [v (and f (widget:frame-data f))] [spans (if v (visible-spans v) '())])
      (when (< index (length spans))
        (let ([name (car (list-ref (columns s (visible-metadata v)) (car (list-ref spans index))))])
          (when (memq name (get (visible-metadata v) 'sortable '())) (toggle-sort! id name))))))

  (edoc "Install list/table compositions, virtual rows, headings and their canonical commands.")
  (define (init!)
    (row:init!)
    (for-each (lambda (kind)
                (widget:register! kind 1
                  (append (layout:container 'y)
                    (list (cons 'prepare data) (cons 'service service!) (cons 'release release!) (cons 'contexts '(widget-table)) (cons 'busy? busy?)
                      (cons 'actions (list (cons 'select select!) (cons 'move move!) (cons 'activate activate!)
                                       (cons 'emphasize emphasize!) (cons 'sort-by sort-by!) (cons 'toggle-sort toggle-sort!) (cons 'set-columns set-columns!))))))) '(table list))
    (for-each (lambda (kind)
                (widget:register! kind 1
                  (append (list (cons 'prepare data) (cons 'viewport viewport) (cons 'render render) (cons 'measure measure) (cons 'decorate decorate) (cons 'event event!) (cons 'pointer-bindings pointer-bindings))
                    (if (eq? kind 'table-body) (list (cons 'focus #t) (cons 'anchor anchor) (cons 'locate locate)) '())))) '(table-heading table-body))
    (for-each (lambda (p) (keymap:bind-default! 'widget-table (car p) (keymap:call move! widget:target (cdr p))))
      '(("UP" . previous) ("DOWN" . next) ("HOME" . first) ("END" . last) ("PGUP" . page-previous) ("PGDN" . page-next)))
    (keymap:bind-default! 'widget-table "RET" (keymap:call activate! widget:target))
    (for-each (lambda (i) (keymap:bind-default! 'widget-table (format "F~a" (+ i 1)) (keymap:call toggle-visible-sort! widget:target i))) (iota 12))))
