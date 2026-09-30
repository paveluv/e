;; Journal records are base data; text, wrapping and styles are head caches.
(import (only (foundation edoc) elibrary))
(elibrary (apps log-view)
  (export copy! create! init! move! scroll! select! set-mark! show!)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (foundation string) string:) (prefix (foundation text) text:)
          (prefix (head edit) edit:) (prefix (head head) head:) (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:) (prefix (head range) range:) (prefix (head render) render:)
          (prefix (head text-layout) text-layout:) (prefix (head widget) widget:) (prefix (head window) window:)
          (prefix (service journal-source) journal-source:) (prefix (service log) log:) (prefix (state view) view:))
  (define (get r k fallback) (cond [(assq k r) => cdr] [else fallback]))
  (define (refuse message) (raise (condition (kernel:make-refusal) (make-message-condition message))))
  ;; State: caret, mark and top are (record-key line character), then mark?
  ;; and follow?. All layout and cached formatted records stay in the head.
  (define-record-type session (fields query token cache (mutable format-basis) (mutable page) (mutable size) (mutable intent) (mutable copy) (mutable status)))
  (define-record-type page (fields generation start total rows lines styles anchors positions frame))
  (define-record-type copying (fields token generation sequence ends (mutable next) (mutable chunks)))
  (define sessions (make-hashtable equal-hash equal?))
  (define dragging #f)
  (define servicing? (make-parameter #f))
  (define (cancel-copy! s)
    (when (session-copy s) (range:release! (copying-token (session-copy s))) (session-copy-set! s #f)))
  (define (release! id)
    (when (equal? id dragging) (set! dragging #f))
    (let ([s (hashtable-ref sessions id #f)])
      (when s (cancel-copy! s) (range:release! (session-token s)) (hashtable-delete! sessions id))))
  (define (formatted s row)
    (let* ([key (cadr row)] [old (hashtable-ref (session-cache s) key #f)])
      (or old
        (let* ([cell (assq 'entry (caddr row))] [entry (and cell (eq? (cadr cell) 'ready) (caddr cell))]
               [stamp (and entry (log:time entry))]
               [date (and stamp (time-utc->date (make-time 'time-utc (mod stamp 1000000000) (div stamp 1000000000))))]
               [prefix (if date (format "~2,'0d:~2,'0d:~2,'0d ~s\t~a " (date-hour date) (date-minute date) (date-second date)
                                  (log:actor entry) (log:component entry)) "")]
               [text (if entry (log:format-entry entry) "[Record exceeds the presentation budget]")]
               [text (if (> (string-length text) 65536) "[Formatted record exceeds the presentation budget]" text)]
               [styler (and entry (log:styler (log:component entry)))]
               [rows (map (lambda (line)
                            (let* ([full (string-append prefix line)] [styles (make-vector (string-length full) 'comment)]
                                   [inner (and styler (guard (ex [else #f]) (styler line)))])
                              (do ([i 0 (+ i 1)]) ((= i (string-length line)))
                                (vector-set! styles (+ (string-length prefix) i)
                                  (if (and (vector? inner) (< i (vector-length inner))) (vector-ref inner i) (if entry 'plain 'ghost))))
                              (list full styles))) (string:lines text))])
          (hashtable-set! (session-cache s) key rows) rows))))
  (define (make-page! s generation start rows total)
    (let ([lines '()] [styles '()] [anchors '()] [positions (make-hashtable equal-hash equal?)] [n 0])
      (for-each (lambda (row)
                  (for-each (lambda (data line)
                              (let ([key (list (cadr row) line)])
                                (hashtable-set! positions key n) (set! n (+ n 1))
                                (set! lines (cons (car data) lines)) (set! styles (cons (cadr data) styles))
                                (set! anchors (cons key anchors)))) (formatted s row) (iota (length (formatted s row))))) rows)
      (let ([lines (list->vector (if (null? lines) '("") (reverse lines)))])
        (make-page generation start total rows lines (list->vector (if (null? styles) '(#()) (reverse styles)))
          (list->vector (if (null? anchors) '(#f) (reverse anchors))) positions (render:prepare #f #f lines 0 '())))))
  (define (anchor p point)
    (let ([a (vector-ref (page-anchors p) (car point))]) (and a (append a (list (cdr point))))))
  (define (position p a)
    (and a (let ([row (hashtable-ref (page-positions p) (list-head a 2) #f)])
             (and row (cons row (min (caddr a) (string-length (vector-ref (page-lines p) row))))))))
  (define (last-point p)
    (let ([row (- (vector-length (page-lines p)) 1)]) (cons row (string-length (vector-ref (page-lines p) row)))))
  (define (metadata s) (get (range:summary (session-query s)) 'value '()))
  (define (state! id p state) (interaction:set-state! head:ui-actor id (page-generation p) state))
  (define (service! id frame)
    (unless (servicing?) (parameterize ([servicing? #t]) (service-once! id frame))))
  (define (service-once! id frame)
    (let* ([d (interaction:snapshot id)] [query (view:source d)] [old (hashtable-ref sessions id #f)])
      (when (and old (not (equal? query (session-query old)))) (release! id) (set! old #f))
      (let* ([s (or old (let ([s (make-session query (range:acquire! query (lambda () (widget:repaint! id #t)))
                                   (make-hashtable equal-hash equal?) (log:presentation-basis) #f '(1 . 1) #f #f 'pending)])
                          (hashtable-set! sessions id s) s))]
             [v (metadata s)] [generation (get v 'generation 0)] [state (view:state d)] [intent (session-intent s)])
        (when frame (session-size-set! s (cons (max 1 (caddr (widget:frame-rect frame))) (cadddr (widget:frame-rect frame)))))
        (when (and intent (not (= (car intent) (view:sequence d)))) (session-intent-set! s #f) (set! intent #f))
        (unless (= (session-format-basis s) (log:presentation-basis))
          (hashtable-clear! (session-cache s)) (session-page-set! s #f) (session-format-basis-set! s (log:presentation-basis)))
        (session-status-set! s (get v 'status 'unavailable))
        (unless (eq? (session-status s) 'ready) (cancel-copy! s))
        (when (eq? (get v 'status #f) 'ready)
          (let* ([total (get v 'count 0)] [follow? (and (list-ref state 4) (not intent))]
                 [a (if (and intent (not (eq? (cadr intent) 'scroll))) (car state) (caddr state))] [key (and a (car a))]
                 [rank (and key (range:locate query generation key))]
                 [at (cond [(and intent (eq? (cadr intent) 'start)) 0]
                       [(or follow? (and intent (eq? (cadr intent) 'finish))) (max 0 (- total 1))]
                       [(not key) 0] [(and rank (eq? (car rank) 'ready)) (or (list-ref rank 3) 0)] [else #f])]
                 [count (min 256 (max 32 (* 2 (cdr (session-size s)))))]
                 [start (max 0 (- (or at 0) (div count 2)))]
                 [reply (and at (range:read query generation start count '(entry)))])
            (range:request! (session-token s) generation start (if at count 0) '(entry) (if key (list key) '()))
            (session-status-set! s (if reply (car reply) 'pending))
            (when (and reply (eq? (car reply) 'ready))
              (let* ([rows (list-ref reply 4)] [prior (session-page s)]
                     [p (if (and prior (= generation (page-generation prior)) (= start (page-start prior)) (equal? rows (page-rows prior))) prior
                          (make-page! s generation start rows total))])
                (unless (eq? p prior)
                  (session-page-set! s p) (widget:repaint! id #t)
                  (when (> (hashtable-size (session-cache s)) 512)
                    (vector-for-each (lambda (key) (unless (exists (lambda (r) (equal? key (cadr r))) rows)
                                                     (hashtable-delete! (session-cache s) key))) (hashtable-keys (session-cache s)))))
                (cond [(or follow? (and intent (eq? (cadr intent) 'finish)))
                       (let* ([point (last-point p)] [a (anchor p point)]
                              [marked? (and intent (caddr intent))]
                              [top (text-layout:move (page-lines p) (page-frame p) (car (session-size s)) point (- 1 (max 1 (cdr (session-size s)))) 0)])
                         (state! id p (list a (if marked? (cadr state) a) (anchor p top) marked? (not marked?))) (endpoint-done! id s intent))]
                  [(and intent (eq? (cadr intent) 'start))
                   (let ([a (anchor p '(0 . 0))] [marked? (caddr intent)])
                     (state! id p (list a (if marked? (cadr state) a) a marked? #f))) (endpoint-done! id s intent)]
                  [else
                   (when (or (not (caddr state)) (and rank (eq? (car rank) 'ready) (not (list-ref rank 3))))
                     (let ([a (anchor p '(0 . 0))]) (state! id p (list a a a #f #f))))
                   (when intent
                     (session-intent-set! s #f)
                     (case (cadr intent)
                       [(rows scroll) (apply advance! id (cadr intent) (caddr intent))]
                       [(character) (apply character! id (caddr intent))]
                       [else (move! id (cadr intent) (caddr intent))]))]))))
          (when (session-copy s) (service-copy! id s)))
        (when (eq? (session-status s) 'unavailable)
          (session-page-set! s #f) (session-intent-set! s #f)))))
  (define (busy? id d)
    (let ([s (hashtable-ref sessions id #f)])
      (or (not s) (eq? (session-status s) 'pending) (and (session-intent s) #t))))
  (define (geometry id d width height)
    (let* ([s (hashtable-ref sessions id #f)] [p (and s (session-page s))] [state (view:state d)]
           [top (and p (position p (caddr state)))])
      (list id p state (and top (cons (car top) (text-layout:segment (text-layout:breaks (vector-ref (page-lines p) (car top)) (max 1 width)) (cdr top)))) (max 1 width) height
        (and s (session-status s)))))
  (define (screen-rows g range)
    (let ([p (cadr g)] [top (cadddr g)] [width (list-ref g 4)])
      (if (not top) '()
        (let loop ([row (car top)] [segment (cdr top)] [y 0] [out '()])
          (if (or (>= y (+ (car range) (cdr range))) (>= row (vector-length (page-lines p)))) (reverse out)
            (let* ([line (vector-ref (page-lines p) row)] [breaks (text-layout:breaks line width)]
                   [a (vector-ref breaks segment)] [next? (< (+ segment 1) (vector-length breaks))]
                   [b (if next? (vector-ref breaks (+ segment 1)) (string-length line))])
              (loop (if next? row (+ row 1)) (if next? (+ segment 1) 0) (+ y 1)
                (if (< y (car range)) out (cons (list y row a b) out)))))))))
  (define (render g d width height range)
    (if (and (eq? (list-ref g 6) 'unavailable) (= (car range) 0)) '("[Journal unavailable]")
      (map (lambda (r) (substring (vector-ref (page-lines (cadr g)) (cadr r)) (caddr r) (cadddr r))) (screen-rows g range))))
  (define (anchor<? a b)
    (or (< (cadar a) (cadar b)) (and (= (cadar a) (cadar b))
                                  (or (< (cadr a) (cadr b)) (and (= (cadr a) (cadr b)) (< (caddr a) (caddr b)))))))
  (define (decorate g d width height range)
    (let* ([p (cadr g)] [state (caddr g)] [ends (and (cadddr state) (car state) (cadr state) (list-sort anchor<? (list-head state 2)))])
      (if (and (eq? (list-ref g 6) 'unavailable) (= (car range) 0)) (list (list (list 0 0 width 1) 'ghost))
        (apply append (map (lambda (r)
                             (let ([row (cadr r)] [a (caddr r)] [b (cadddr r)])
                               (map (lambda (i)
                                      (let* ([at (anchor p (cons row i))]
                                             [face (if (and ends (not (anchor<? at (car ends))) (anchor<? at (cadr ends))) 'selection
                                                     (vector-ref (vector-ref (page-styles p) row) i))]
                                             [x (- (render:column (page-frame p) row i) (render:column (page-frame p) row a))]
                                             [end (- (render:column (page-frame p) row (+ i 1)) (render:column (page-frame p) row a))])
                                        (list (list x (car r) (- end x) 1) face))) (map (lambda (i) (+ i a)) (iota (- b a)))))) (screen-rows g range))))))
  (define (caret g d width height)
    (let ([p (cadr g)] [top (cadddr g)])
      (and p top (position p (car (caddr g)))
        (text-layout:locate (page-lines p) (page-frame p) (list-ref g 4) top 0 (position p (car (caddr g)))))))
  (define (context id)
    (let* ([s (hashtable-ref sessions id #f)] [d (interaction:snapshot id)] [p (and s (session-page s))])
      (unless (and p d (eq? (view:kind d) 'log) (equal? (view:source d) (session-query s))) (refuse "Journal records are pending or the receiver changed"))
      (values s d p)))
  (define (publish! id s d p caret fixed top marked?)
    (let* ([width (car (session-size s))] [state (view:state d)]
           [address (if top (cons (car top) (text-layout:segment (text-layout:breaks (vector-ref (page-lines p) (car top)) width) (cdr top))) '(0 . 0))])
      (let-values ([(point address left) (text-layout:scroll (page-lines p) (page-frame p) width width (cdr (session-size s)) 0 address 0 caret 2)])
        (state! id p (list (anchor p point) fixed (anchor p (text-layout:anchor (page-lines p) width address)) marked? #f)))))
  (define (queue! id s kind arguments)
    (session-intent-set! s (list (view:sequence (interaction:snapshot id)) kind arguments)) (head:wake-main!))
  (define (defer-endpoint! s d kind arguments)
    (let ([intent (session-intent s)])
      (and intent (= (car intent) (view:sequence d)) (memq (cadr intent) '(start finish))
        (let* ([prior (and (= (length intent) 4) (list-ref intent 3))]
               [arguments (if (and prior (eq? kind (car prior)))
                            (case kind
                              [(rows scroll) (list (+ (car arguments) (cadr prior)) (cadr arguments))]
                              [(character) (if (eq? (car arguments) (cadr prior))
                                             (list (car arguments) (cadr arguments) (+ (caddr arguments) (cadddr prior))) arguments)]
                              [else arguments]) arguments)])
          (session-intent-set! s (append (list-head intent 3) (list (cons kind arguments)))) #t))))
  (define (endpoint-done! id s intent)
    (session-intent-set! s #f)
    (when (and intent (= (length intent) 4))
      (let ([after (list-ref intent 3)])
        (case (car after)
          [(rows scroll) (apply advance! id (car after) (cdr after))]
          [(character) (apply character! id (cdr after))]))))
  (define (more? p delta)
    (if (negative? delta) (> (page-start p) 0) (< (+ (page-start p) (length (page-rows p))) (page-total p))))
  (define (advance! id kind delta extend)
    (let-values ([(s d p) (context id)])
      (unless (defer-endpoint! s d kind (list delta extend))
        (let* ([state (view:state d)] [scroll? (eq? kind 'scroll)] [at (position p (if scroll? (caddr state) (car state)))]
               [pending (session-intent s)]
               [delta (+ delta (if (and pending (= (car pending) (view:sequence d)) (eq? kind (cadr pending))) (caaddr pending) 0))]
               [width (car (session-size s))])
          (session-intent-set! s #f)
          (if (not at) (queue! id s kind (list delta extend))
            (let* ([next (text-layout:move (page-lines p) (page-frame p) width at delta
                           (if scroll? 0 (render:column (page-frame p) (car at) (cdr at))))]
                   [address (cons (car at) (text-layout:segment (text-layout:breaks (vector-ref (page-lines p) (car at)) width) (cdr at)))]
                   [remaining (- delta (text-layout:distance (page-lines p) width address next))]
                   [marked? (or extend (cadddr state))])
              (if scroll? (state! id p (list (car state) (cadr state) (anchor p next) (cadddr state) #f))
                (publish! id s d p next (if marked? (cadr state) (anchor p next)) (position p (caddr state)) marked?))
              (when (and (not (zero? remaining)) (more? p remaining)) (queue! id s kind (list remaining extend)))))))))
  (define (character! id direction extend count)
    (let-values ([(s d p) (context id)])
      (unless (defer-endpoint! s d 'character (list direction extend count))
        (let* ([state (view:state d)] [at (position p (car state))] [pending (session-intent s)]
               [count (+ count (if (and pending (= (car pending) (view:sequence d)) (eq? (cadr pending) 'character)
                                     (eq? direction (caaddr pending))) (caddr (caddr pending)) 0))])
          (session-intent-set! s #f)
          (if (not at) (queue! id s 'character (list direction extend count))
            (let loop ([point at] [left count])
              (let ([next (text-layout:adjacent (page-lines p) point direction)])
                (if (or (zero? left) (equal? point next))
                  (let ([marked? (or extend (cadddr state))])
                    (publish! id s d p point (if marked? (cadr state) (anchor p point)) (position p (caddr state)) marked?)
                    (when (and (> left 0) (more? p (if (eq? direction 'left) -1 1)))
                      (queue! id s 'character (list direction extend left))))
                  (loop next (- left 1))))))))))

  (edoc "Create an unmounted journal view with independent logical selection, scrolling and tail following. False shows all components."
        (component (or symbol #f) "component filter") (returns model) (public))
  (define (create! component)
    (let ([query (journal-source:create! head:ui-actor component)])
      (view:create! head:ui-actor query 'log 1 '() '(#f #f #f #f #t) query)))

  (edoc "Select journal anchors (record-key line character), independent of wrapping."
        (receiver id (view log)) (id model "journal view") (caret list "active anchor") (fixed list "fixed anchor"))
  (define (select! id caret fixed)
    (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
    (unless (and (for-all (lambda (a) (and (list? a) (= (length a) 3) (list? (car a)) (= (length (car a)) 2)
                                        (string? (caar a)) (natural? (cadar a)) (for-all natural? (cdr a)))) (list caret fixed))
              (equal? (caar caret) (caar fixed))) (error 'select! "expected anchors in the same journal lifetime"))
    (let-values ([(s d p) (context id)])
      (let ([point (position p caret)])
        (unless point (refuse "Journal position is outside the acquired records"))
        (publish! id s d p point fixed (position p (caddr (view:state d))) (not (equal? caret fixed))))))

  (edoc "Move within this journal. Finish resumes tail following; other movement keeps the reader's record anchors."
        (receiver id (view log)) (id model "journal view") (direction symbol "text motion") (extend (list-of boolean) "optional selection extension"))
  (define (move! id direction . extend)
    (unless (and (memq direction '(up down left right home end start finish page-up page-down))
              (<= (length extend) 1) (for-all boolean? extend)) (error 'move! "invalid motion"))
    (let-values ([(s d p) (context id)])
      (let* ([state (view:state d)] [at (or (position p (car state)) (position p (caddr state)) '(0 . 0))]
             [marked? (or (cadddr state) (and (pair? extend) (car extend)))]
             [width (car (session-size s))]
             [delta (case direction [(up) -1] [(down) 1] [(page-up) (- (max 1 (- (cdr (session-size s)) 1)))] [(page-down) (max 1 (- (cdr (session-size s)) 1))] [else 0])]
             [point (case direction [(start finish) #f] [(left right) (text-layout:adjacent (page-lines p) at direction)]
                      [(home) (cons (car at) 0)] [(end) (cons (car at) (string-length (vector-ref (page-lines p) (car at))))]
                      [else (text-layout:move (page-lines p) (page-frame p) width at delta (render:column (page-frame p) (car at) (cdr at)))])])
        (cond [(not point) (queue! id s direction marked?)]
          [(not (zero? delta)) (advance! id 'rows delta marked?)]
          [(memq direction '(left right)) (character! id direction marked? 1)]
          [else (session-intent-set! s #f)
            (publish! id s d p point (if marked? (cadr state) (anchor p point)) (position p (caddr state)) marked?)]))))

  (edoc "Scroll this journal by displayed rows without moving its caret or following the tail."
        (receiver id (view log)) (id model "journal view") (delta integer "row displacement"))
  (define (scroll! id delta)
    (unless (and (integer? delta) (exact? delta)) (error 'scroll! "expected a row displacement"))
    (advance! id 'scroll delta #f))

  (edoc "Set or clear this journal's selection mark." (receiver id (view log)) (id model "journal view") (active boolean "mark activity"))
  (define (set-mark! id active)
    (unless (boolean? active) (error 'set-mark! "expected boolean"))
    (let-values ([(s d p) (context id)])
      (let ([state (view:state d)]) (state! id p (list (car state) (car state) (caddr state) active #f)))))

  (edoc "Copy selected journal text by bounded record pages. Changing the selection or result cancels the request; no partial text is copied."
        (receiver id (view log)) (id model "journal view"))
  (define (copy! id)
    (let-values ([(s d p) (context id)])
      (unless (and (cadddr (view:state d)) (for-all values (list-head (view:state d) 2))) (refuse "No journal selection"))
      (cancel-copy! s)
      (session-copy-set! s (make-copying (range:acquire! (session-query s) (lambda () (head:wake-main!))) (page-generation p)
                             (view:sequence d) (list-sort anchor<? (list-head (view:state d) 2)) #f '()))
      (head:wake-main!)))
  (define (service-copy! id s)
    (let* ([job (session-copy s)] [query (session-query s)] [g (copying-generation job)] [ends (copying-ends job)]
           [ranks (map (lambda (a) (range:locate query g (car a))) ends)])
      (cond [(or (not (= g (get (metadata s) 'generation -1))) (not (= (copying-sequence job) (view:sequence (interaction:snapshot id))))) (cancel-copy! s)]
        [(not (for-all (lambda (r) (eq? (car r) 'ready)) ranks))
         (range:request! (copying-token job) g 0 0 '(entry) (map car ends))]
        [(not (for-all (lambda (r) (list-ref r 3)) ranks)) (cancel-copy! s)]
        [else
         (let* ([start (or (copying-next job) (list-ref (car ranks) 3))] [end (list-ref (cadr ranks) 3)]
                [count (min 64 (+ 1 (- end start)))] [reply (range:read query g start count '(entry))])
           (range:request! (copying-token job) g start count '(entry) (map car ends))
           (case (car reply)
             [(unavailable) (cancel-copy! s)]
             [(ready)
              (let* ([p (make-page! s g start (list-ref reply 4) (list-ref reply 5))]
                     [from (if (= start (list-ref (car ranks) 3)) (position p (car ends)) '(0 . 0))]
                     [done? (> (+ start count) end)] [to (if done? (position p (cadr ends)) (last-point p))])
                (if (not (and from to)) (cancel-copy! s)
                  (begin
                    (copying-chunks-set! job (cons (text:to-string (list->vector (text:extract (page-lines p)
                                                                                   (text:make-span (car from) (cdr from) (car to) (cdr to)))) #f) (copying-chunks job)))
                    (if done? (let ([text (string:join (reverse (copying-chunks job)) "\n")]) (cancel-copy! s) (edit:copy-text! text))
                      (copying-next-set! job (+ start count))))))]))])))
  (define (pointer-point frame x y)
    (let* ([g (widget:frame-data frame)] [p (cadr g)] [r (assv y (screen-rows g (cons y 1)))])
      (and r (anchor p (cons (cadr r) (min (cadddr r) (render:character (page-frame p) (cadr r)
                                                        (+ x (render:column (page-frame p) (cadr r) (caddr r))))))))))
  (define (pointer-bindings frame x y)
    (let ([a (pointer-point frame x y)] [id (widget:frame-id frame)] [state (view:state (widget:frame-descriptor frame))])
      (if (not a) '() (list (list '(click primary ()) (keymap:call select! id a a))
                        (list '(click primary (shift)) (keymap:call select! id a (cadr state)))
                        (list '(drag primary ()) (keymap:call select! id a (cadr state)))))))
  (define (event! id source d event)
    (case (car event)
      [(cancel blur) (when (equal? dragging id) (set! dragging #f)) #t]
      [(pointer)
       (cond [(eq? (cadr event) 'release) (when (equal? dragging id) (set! dragging #f)) #t]
         [(and (eq? (caddr event) 'primary) (or (eq? (cadr event) 'press) (and (eq? (cadr event) 'move) (equal? dragging id))))
          (let* ([extend? (or (eq? (cadr event) 'move) (memq 'shift (cadddr event)))]
                 [binding (assoc (if extend? '(click primary (shift)) '(click primary ()))
                            (pointer-bindings (widget:event-frame) (list-ref event 4) (list-ref event 5)))])
            (and binding (begin (keymap:run! (cadr binding)) (set! dragging id) (widget:capture! id) #t)))] [else #f])]
      [else #f]))
  (define (default! component)
    (window:tool! (if component (format "log ~a" component) "log") (lambda (commands) (create! component))))

  (edoc "Pop up the default journal or a component-filtered journal. The same retained tool resumes its selection and following state."
        (component (list-of symbol) "optional component filter") (returns model) (public))
  (define (show! . component)
    (unless (and (<= (length component) 1) (for-all symbol? component)) (error 'show! "expected at most one component"))
    (let ([root (default! (and (pair? component) (car component)))]) (window:pop-up-or-reuse! (widget:host root)) root))

  (edoc "Register the journal widget and named text commands without opening a tool." (public))
  (define (init!)
    (widget:register! 'log 1
      (list (cons 'service service!) (cons 'release release!) (cons 'busy? busy?) (cons 'focus #t) '(contexts . (widget-log))
        (cons 'prepare (lambda (id source inputs) id)) (cons 'viewport (lambda (id d w h range) (geometry id d w h)))
        (cons 'render render) (cons 'decorate decorate) (cons 'caret caret) (cons 'event event!) (cons 'pointer-bindings pointer-bindings)
        (cons 'actions (list (cons 'move move!) (cons 'scroll scroll!) (cons 'select select!) (cons 'copy copy!) (cons 'set-mark set-mark!)))))
    (for-each (lambda (p) (keymap:bind-default! 'widget-log (car p) (keymap:call move! widget:target (cadr p))))
      '(("UP" up) ("DOWN" down) ("LEFT" left) ("RIGHT" right) ("HOME" home) ("END" end) ("C-p" up) ("C-n" down)
        ("C-b" left) ("C-f" right) ("C-a" home) ("C-e" end) ("M-<" start) ("M->" finish) ("C-HOME" start) ("C-END" finish)
        ("PAGEUP" page-up) ("PAGEDOWN" page-down) ("M-v" page-up) ("C-v" page-down)))
    (for-each (lambda (p) (keymap:bind-default! 'widget-log (car p) (keymap:call move! widget:target (cadr p) #t)))
      '(("S-UP" up) ("S-DOWN" down) ("S-LEFT" left) ("S-RIGHT" right)))
    (keymap:bind-default! 'widget-log "C-@" (keymap:call set-mark! widget:target #t))
    (keymap:bind-default! 'widget-log "C-g" (keymap:call set-mark! widget:target #f))
    (keymap:bind-default! 'widget-log "M-w" (keymap:call copy! widget:target))))
