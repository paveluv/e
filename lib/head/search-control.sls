;; Incremental search is an entry composition targeting an explicit editor.
(import (only (foundation edoc) elibrary))
(elibrary (head search-control)
  (export accept! cancel! create! create-preview! init! repeat! retarget! set-needle! toggle-case!)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (foundation string) string:) (prefix (foundation text) text:)
          (prefix (head editor) editor:)
          (prefix (head editor-state) editor-state:) (prefix (head entry) entry:)
          (prefix (head head) head:) (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:) (prefix (head layout) layout:)
          (prefix (head text-control) text-control:) (prefix (head text-source) text-source:)
          (prefix (head widget) widget:) (prefix (service log) log:)
          (prefix (service search-request) search-request:)
          (prefix (state connection) connection:) (prefix (state model) model:) (prefix (state view) view:) (prefix (sys glyph) glyph:))
  (define (get r k) (cdr (assq k r)))
  (define-record-type runtime
    (fields request (mutable target) (mutable signature) (mutable pending) (mutable hit) (mutable visible) (mutable last-result) (mutable finishing?)))
  (define live (make-hashtable equal-hash equal?))
  (define closing (make-hashtable equal-hash equal?))
  (define (refuse message) (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (text id)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (let ([input (assq 'needle inputs)])
        (unless (and input (eq? (cadr input) 'ready)) (refuse "Search needle is unavailable"))
        ;; Request generation and target sequence fence late results. The
        ;; semantic input value avoids invalidation by its own publication.
        (values (caddr input) (caddr input)))))
  (define (target-context target)
    (let-values ([(source d) (text-control:context target 'editor 'current)])
      (let ([ps (editor-state:points (text-control:mirror source) (text-control:revision source) d)])
        (unless ps (refuse "Search origin history is unavailable"))
        (values source d (car ps)))))
  (define (home source point) (list (text-source:id (text-control:mirror source)) (text-control:revision source) point))
  (define (rebase source home)
    (and (= (car home) (text-source:id (text-control:mirror source)))
      (let ([ps (text-source:rebase (list (caddr home))
                  (text-source:changes (text-control:mirror source) (cadr home) (text-control:revision source)))])
        (and ps (car ps)))))
  (define (shown target)
    (define (find frame)
      (if (equal? target (widget:frame-id frame)) frame (exists find (widget:frame-children frame))))
    (exists (lambda (placement) (find (car placement))) (widget:shown)))
  (define (visible target source point)
    (let ([frame (shown target)])
      (if (not frame) (text:span->datum (text:make-span (car point) (cdr point) (car point) (cdr point)))
        (let* ([size (widget:frame-rect frame)] [from (editor:frame-hit frame 0 0)]
               [to (editor:frame-hit frame (max 0 (- (caddr size) 1)) (max 0 (- (cadddr size) 1)))]
               [old (text-control:revision (widget:frame-source frame))]
               [ps (text-source:rebase (list from to) (text-source:changes (text-control:mirror source) old (text-control:revision source)))])
          (if (not ps) (list (car point) (cdr point) (car point) (cdr point))
            (let* ([from (car ps)] [to (cadr ps)] [line (vector-ref (text-control:lines source) (car to))])
              (list (car from) (cdr from) (car to) (min (string-length line) (+ (cdr to) 1)))))))))
  (define (fold? policy needle)
    (or (eq? policy 'fold) (and (eq? policy 'smart) (for-all (lambda (c) (not (char-upper-case? c))) (string->list needle)))))
  (define (request target source d point needle policy direction summary? overlap?)
    (map cons '(target document basis sequence start needle fold? visible direction summary? overlap?)
      (list target (text-source:id (text-control:mirror source)) (text-control:revision source)
        (view:sequence d) point needle (fold? policy needle) (visible target source point) direction summary? overlap?)))

  (edoc "Create a search entry for an explicit mounted editor. The host binds finished (accepted? origin-target needle); nested hosts may keep the target pinned. Policy is smart, fold or exact; remembered text is used by repeat on an empty entry."
        (target model "editor view") (policy (one-of smart fold exact) "case policy") (remembered string "previous needle")
        (commands list "explicit finished command") (returns model "unmounted search view"))
  (define (create! target policy remembered commands)
    (build target policy remembered commands #t #f))

  (edoc "Create a search controller with a status line and externally supplied needle. It uses the same search request, typed needle input and explicit editor as interactive search, without allocating an authored entry draft."
        (target model "mounted preview editor") (policy (one-of smart fold exact) "case policy") (commands list "host targets") (owner model "lifetime owner") (returns model) (public))
  (define (create-preview! target policy commands owner) (build target policy "" commands #f owner))
  (define (build target policy remembered commands entry? owner)
    (unless (and (memq policy '(smart fold exact)) (string? remembered)) (error 'create! "invalid search options"))
    (let-values ([(source d point) (target-context target)])
      (let ([query (search-request:create! head:ui-actor (request target source d point "" policy 'next (not entry?) entry?) entry?)])
        (guard (ex [else (search-request:close! head:ui-actor query) (raise ex)])
          (let* ([record (caddr (caadr (model:snapshots (list query))))] [draft (get (get record 'value) 'draft)]
                 [options (list '(spacing . normal) (cons 'commands commands) (cons 'remembered remembered) (cons 'summary? (not entry?)) (cons 'overlap? entry?))]
                 [root (view:create! head:ui-actor query 'search 1 options
                         (list target policy 0 'search (home source point) "") (or owner query))]
                 [label (view:create! head:ui-actor query 'search-label 1 '() '() root)]
                 [input (and entry? (view:create! head:ui-actor draft 'entry 1 '() '((0 . 0) (0 . 0)) root))])
            (view:arrange! head:ui-actor
              (list (list root 0 (append (list (list 'label label 'fit)) (if input (list (list 'entry input '(grow 1))) '())) options)) '())
            (when input (connection:bind! head:ui-actor root (list (list root 'needle #f (list input 'text)))))
            root)))))

  (edoc "Set a search controller's needle through its owned entry or logical input state. This changes no source document."
        (receiver id (view search)) (id model "search controller") (needle string "single-line needle"))
  (define (set-needle! id needle)
    (unless (and (string? needle) (not (string:search needle "\n" 0 (string-length needle)))) (error 'set-needle! "expected a single-line needle"))
    (let* ([d (interaction:snapshot id)] [input (assq 'entry (view:children d))])
      (if input (entry:set-text! (cadr input) needle)
        (let ([state (view:state d)])
          (unless (equal? needle (list-ref state 5))
            (interaction:set-state! head:ui-actor id #f (append (list-head state 5) (list needle))))))))
  (define (disconnect! r)
    (when (and (runtime-target r) (interaction:snapshot (runtime-target r)))
      (guard (ex [else (void)])
        (interaction:bind! head:ui-actor (runtime-target r)
          (list (list (runtime-target r) 'annotations (list (runtime-request r) 'annotations) #f)))))
    (runtime-target-set! r #f))
  (define (release! id)
    (let ([r (hashtable-ref live id #f)])
      (when r
        (hashtable-delete! live id)
        (hashtable-set! closing id #t)
        (head:run-on-main! (lambda () (disconnect! r) (search-request:close! head:ui-actor (runtime-request r)) (hashtable-delete! closing id))))))
  (define (connect! r target)
    (unless (equal? target (runtime-target r))
      (disconnect! r)
      (let-values ([(status bindings) (interaction:bind! head:ui-actor target (list (list target 'annotations #f (list (runtime-request r) 'annotations))))])
        (unless (eq? status 'applied) (refuse "The editor's annotations are already connected")))
      (runtime-target-set! r target) (runtime-signature-set! r #f) (runtime-hit-set! r #f)))
  (define (service! id frame)
    (guard (ex [else (log:add! 'search-control:service! (kernel:condition-text ex)) (release! id)])
      (let* ([d (interaction:snapshot id)] [query (view:source d)]
             [record (model:snapshot query)] [v (and record (get record 'value))]
             [r (and v (not (hashtable-ref closing id #f)) (equal? (get v 'owner) head:ui-actor)
                  (or (hashtable-ref live id #f)
                    (let ([r (make-runtime query #f #f #f #f #f #f #f)]) (hashtable-set! live id r) r)))])
        (when r
          (let* ([state (view:state d)] [target (car state)])
            (connect! r target)
            (let-values ([(needle draft-basis) (text id)] [(source target-d point) (target-context target)])
              (let* ([signature (list target (cadr state) (caddr state) needle)]
                     [changed? (not (equal? signature (runtime-signature r)))]
                     [viewport (list (text-control:revision source) (visible target source point))]
                     [old (runtime-signature r)] [hit (and (runtime-hit r) (rebase source (runtime-hit r)))])
                (when (or changed? (not (equal? viewport (runtime-visible r))))
                  (let* ([repeat? (and old (not (= (caddr state) (caddr old))) (memq (cadddr state) '(repeat previous)))]
                         [home (or (rebase source (list-ref state 4)) point)]
                         [start (cond [(and repeat? hit) (if (eq? (cadddr state) 'previous) hit (cons (car hit) (+ 1 (cdr hit))))]
                                  [(or (not old) (not (eq? (cadr old) (cadr state))) (< (string-length needle) (string-length (list-ref old 3)))) home]
                                  [else (or hit home)])]
                         [q (request target source target-d start needle (cadr state)
                              (if (and repeat? (eq? (cadddr state) 'previous)) 'previous 'next) (get (view:options d) 'summary?) (get (view:options d) 'overlap?))]
                         [generation (search-request:configure! head:ui-actor query
                                       (max (get v 'generation) (if (runtime-pending r) (car (runtime-pending r)) 0)) q)])
                    (when generation
                      (runtime-signature-set! r signature) (runtime-visible-set! r viewport)
                      (runtime-pending-set! r (list generation (view:generation target-d) (view:sequence target-d) draft-basis changed?)))))
                (let* ([pending (runtime-pending r)] [result (get v 'result)]
                       [stamp (list (get v 'generation) result)])
                  (when (and (eq? (get v 'status) 'unavailable) pending (= (car pending) (get v 'generation)))
                    (runtime-pending-set! r #f)
                    (runtime-last-result-set! r stamp))
                  (when (and (eq? (get v 'status) 'ready) result (not (equal? stamp (runtime-last-result r)))
                          (let ([shown (shown target)])
                            (and shown (= (view:generation (widget:frame-descriptor shown)) (view:generation target-d))
                              (= (view:sequence (widget:frame-descriptor shown)) (view:sequence target-d)))))
                    (runtime-last-result-set! r stamp)
                    (when (and pending (= (car pending) (get v 'generation)))
                      (runtime-pending-set! r #f)
                      (when (and (equal? draft-basis (list-ref pending 3)) (= (view:generation target-d) (cadr pending))
                              (= (view:sequence target-d) (caddr pending)))
                        (let* ([p (get result 'hit)]
                               [p (and p (rebase source (list (get (get v 'request) 'document) (get result 'basis) p)))])
                          (when p (runtime-hit-set! r (home source p)))
                          (when (list-ref pending 4)
                            (cond [p (editor:move! target (cons (car p) (+ (cdr p) (string-length needle))))]
                              [(string=? needle "") (let ([origin (rebase source (list-ref state 4))]) (when origin (editor:move! target origin)))]))))))
                  (when (and (eq? (cadddr state) 'accept) (not (runtime-finishing? r))
                          (not (runtime-pending r)) (equal? signature (runtime-signature r))
                          (memq (get v 'status) '(ready unavailable)) (equal? stamp (runtime-last-result r)))
                    (runtime-finishing?-set! r #t)
                    (head:run-on-main!
                      (lambda ()
                        (when (eq? r (hashtable-ref live id #f))
                          (runtime-finishing?-set! r #f)
                          (let-values ([(current basis) (text id)])
                            (when (equal? basis draft-basis) (finish! id #t)))))))))))))))

  (define (finish! id accepted?)
    (let* ([d (interaction:snapshot id)] [query (view:source d)] [r (model:snapshot query)]
           [origin (and r (get (get r 'value) 'origin))])
      (let-values ([(needle revision) (text id)])
        (when (and origin (not accepted?) (interaction:snapshot (get origin 'target)))
          (guard (ex [else (void)])
            (let-values ([(source d point) (target-context (get origin 'target))])
              (let ([point (rebase source (list (get origin 'document) (get origin 'basis) (get origin 'start)))])
                (when point (editor:move! (get origin 'target) point))))))
        (when (assq 'finished (widget:commands id)) (widget:invoke! id 'finished accepted? (and origin (get origin 'target)) needle))
        (release! id))))

  (edoc "Accept search through the host command after its latest request settles. An optional false accepts the currently shown position immediately, for host navigation keys."
        (receiver id (view search)) (id model "search view") (settle (list-of boolean) "wait for current needle, true by default"))
  (define (accept! id . settle)
    (unless (and (<= (length settle) 1) (for-all boolean? settle)) (error 'accept! "expected an optional boolean"))
    (if (and (pair? settle) (not (car settle))) (finish! id #t)
      (let* ([d (interaction:snapshot id)] [state (view:state d)])
        (interaction:set-state! head:ui-actor id #f (list (car state) (cadr state) (caddr state) 'accept (list-ref state 4) (list-ref state 5))))))

  (edoc "Cancel search, safely rebase and restore its captured origin, then finish through the host command. Text edits are never undone."
        (receiver id (view search)) (id model "search view"))
  (define (cancel! id) (finish! id #f))

  (edoc "Repeat search, using the remembered needle when input is empty. Direction defaults to next." (receiver id (view search)) (id model "search view")
        (direction (list-of (one-of next previous)) "optional direction"))
  (define (repeat! id . direction)
    (unless (and (<= (length direction) 1) (for-all (lambda (x) (memq x '(next previous))) direction)) (error 'repeat! "expected next or previous"))
    (let ([d (interaction:snapshot id)])
      (let-values ([(needle revision) (text id)])
        (when (string=? needle "") (set-needle! id (get (view:options d) 'remembered))))
      (let ([state (view:state (interaction:snapshot id))])
        (interaction:set-state! head:ui-actor id #f (list (car state) (cadr state) (+ 1 (caddr state))
                                                      (if (equal? direction '(previous)) 'previous 'repeat) (list-ref state 4) (list-ref state 5))))))

  (edoc "Toggle this search's effective case policy between exact and folded matching." (receiver id (view search)) (id model "search view"))
  (define (toggle-case! id)
    (let* ([d (interaction:snapshot id)] [state (view:state d)])
      (let-values ([(needle revision) (text id)])
        (interaction:set-state! head:ui-actor id #f
          (list (car state) (if (fold? (cadr state) needle) 'exact 'fold) (+ 1 (caddr state)) 'search (list-ref state 4) (list-ref state 5))))))

  (edoc "Explicitly retarget search to another mounted editor. Pending results for the previous target cannot move the new one; cancellation retains the original receiver."
        (receiver id (view search)) (id model "search view") (target model "new editor"))
  (define (retarget! id target)
    (let* ([d (interaction:snapshot id)] [state (view:state d)])
      (unless (equal? target (car state))
        (let-values ([(source target-d point) (target-context target)])
          (interaction:set-state! head:ui-actor id #f
            (list target (cadr state) (+ 1 (caddr state)) 'search (home source point) (list-ref state 5)))))))

  (define (label id source inputs)
    (let ([v (and source (get source 'value))])
      (if (not v) "Search unavailable:"
        (let ([result (get v 'result)] [request (get v 'request)])
          (if (get request 'summary?)
            (cond [(eq? (get v 'status) 'unavailable) "[Search unavailable]"]
              [(and result (get result 'count))
               (if (get result 'ordinal) (format "[~a of ~a]" (get result 'ordinal) (get result 'count)) "[no match]")]
              [(and result (get result 'hit)) "[match]"] [else ""])
            (string-append
              (if (or (eq? (get v 'status) 'unavailable)
                    (and result (not (string=? (get request 'needle) "")) (not (get result 'hit)))) "Failing " "")
              "I-search" (if (get request 'fold?) "" " (exact)") ":"))))))
  (define (capture! id source d event)
    (when (and (eq? (cadddr (view:state d)) 'accept)
            (or (eq? (car event) 'text) (and (eq? (car event) 'key) (> (length event) 2) (caddr event))))
      (refuse "Search is finishing; C-g cancels")) #f)

  (edoc "Register the reusable search entry and its named commands." (public))
  (define (init!)
    (widget:register! 'search-label 1
      (list (cons 'prepare label)
        (cons 'render (lambda (data d width height range) (if (zero? (car range)) (list (glyph:fit data width)) '())))
        (cons 'measure (lambda (data d axis cross child) (if (eq? axis 'y) '(1 1) (list 0 (glyph:cells data)))))
        (cons 'decorate (lambda (data d width height range) (list (list (list 0 0 width height)
                                                                    (if (and (> (string-length data) 0) (char=? (string-ref data 0) #\[)) 'ghost 'chrome)))))))
    (widget:register! 'search 1
      (append (layout:container 'x)
        (list (cons 'capture-contexts '(widget-search)) (cons 'capture-event capture!) (cons 'service service!) (cons 'release release!)
          (cons 'actions (list (cons 'accept accept!) (cons 'cancel cancel!) (cons 'repeat repeat!) (cons 'toggle-case toggle-case!) (cons 'retarget retarget!))))))
    (for-each (lambda (p) (keymap:bind-default! 'widget-search (car p) (keymap:call (cdr p) widget:target)))
      (list (cons "RET" accept!) (cons "ESC" accept!) (cons "C-g" cancel!) (cons "C-s" repeat!) (cons "M-c" toggle-case!)))))
