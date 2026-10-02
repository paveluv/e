;; One-time ingress for the last window checkpoint format. No old host records.
(import (only (foundation edoc) elibrary))
(elibrary (state screen-import)
  (export checkpoint create!)
  (import (chezscheme) (prefix (core handle) handle:) (prefix (core kernel) kernel:)
    (prefix (core property) property:) (prefix (foundation digest) digest:) (prefix (foundation text) text:)
    (prefix (state actor) actor:) (prefix (state construction) construction:) (prefix (state manager) manager:)
    (prefix (state model) model:) (prefix (state store) store:) (prefix (state view) view:))
  (define lock (unbox (kernel:persistent-cell 'screen-import-lock make-mutex)))
  (define (natural? x) (and (integer? x) (exact? x) (>= x 0)))
  (define (position? x) (and (pair? x) (natural? (car x)) (natural? (cdr x))))
  (define (valid! state)
    ;; Validate before promoting even one authored document. Unknown extension
    ;; references are opaque; known reference fields alone are interpreted.
    (unless (and (list? state) (= (length state) 5) (equal? (list-head state 2) '(screen 6))
              (natural? (caddr state)) (> (caddr state) 0) (list? (list-ref state 4)))
      (error 'create! "unsupported screen checkpoint; original retained"))
    (let ([entries (list-ref state 4)] [numbers '()] [editors '()])
      (for-each
        (lambda (entry)
          (unless (and (list? entry) (= (length entry) 3) (boolean? (cadr entry))
                    (list? (caddr entry)) (for-all (lambda (p) (and (pair? p) (position? (cdr p)))) (caddr entry)))
            (error 'create! "invalid saved document placement"))
          (let ([ref (car entry)])
            (when ref
              (unless (and (list? ref) (pair? ref) (symbol? (car ref))) (error 'create! "invalid saved reference"))
              (case (car ref)
                [(shared) (unless (and (= (length ref) 3) (handle:buffer? (cadr ref)) (natural? (caddr ref))) (error 'create! "invalid shared reference"))]
                [(widget) (unless (and (= (length ref) 2) (model:reference? (cadr ref))) (error 'create! "invalid widget reference"))]
                [(local)
                 (unless (and (= (length ref) 5) (string? (cadr ref)) (natural? (caddr ref))
                           (list? (list-ref ref 4)) (for-all text:line? (list-ref ref 4)))
                   (error 'create! "invalid saved local text"))
                 (property:validate (cadddr ref))])))) entries)
      (let walk ([node (cadddr state)])
        (unless (and (list? node) (pair? node)) (error 'create! "invalid saved layout"))
        (case (car node)
          [(window)
           (unless (and (= (length node) 8) (for-all natural? (list-head (cdr node) 4)) (> (cadr node) 0)
                     (< (caddr node) (length entries)) (not (memv (cadr node) numbers))
                     (memq (list-ref node 5) '(default #t #f)) (memq (list-ref node 6) '(default #t #f))
                     (list? (list-ref node 7))) (error 'create! "invalid saved window"))
           (set! numbers (cons (cadr node) numbers))
           (let ([documents '()])
             (for-each (lambda (p)
                         (unless (and (pair? p) (handle:buffer? (car p)) (model:reference? (cdr p))
                                   (not (member (car p) documents)) (not (member (cdr p) editors)))
                           (error 'create! "invalid retained editor reference"))
                         (set! documents (cons (car p) documents)) (set! editors (cons (cdr p) editors))) (list-ref node 7)))]
          [(split)
           (unless (and (= (length node) 6) (memq (cadr node) '(right below))
                     (for-all (lambda (n) (and (real? n) (rational? n) (> n 0))) (list (caddr node) (cadddr node))))
             (error 'create! "invalid saved split"))
           (walk (list-ref node 4)) (walk (list-ref node 5))]
          [else (error 'create! "invalid saved topology")]))
      (unless (memv (caddr state) numbers) (error 'create! "saved selection is absent"))))
  (define (fingerprint state)
    (parameterize ([print-length #f] [print-level #f] [print-graph #f]) (digest:hex (digest:sha256 (format "~s" state)))))

  (edoc "Validate recovery witnesses in a candidate descriptor tree and return their unchanged checkpoint input, or false after consumption or without an import. No layout data is interpreted at admission."
    (actor actor "named head") (tree list "canonical descriptor rows") (returns datum))
  (define (checkpoint actor tree)
    (let ([witnesses (filter values
                       (map (lambda (row)
                              (let ([d (cdr row)])
                                (and (eq? (view:kind d) 'window-manager) (= (view:schema d) 2)
                                  (assq 'import-checkpoint (view:options d))))) tree))])
      (and (pair? witnesses)
        (let ([state (actor:checkpoint actor)])
          (when state
            (unless (for-all (lambda (w) (equal? (cdr w) (fingerprint state))) witnesses)
              (error 'checkpoint "saved screen changed during import; original retained"))) state))))
  (define (promote! actor digest slot ref)
    (let* ([origin (list 'screen-6 actor digest slot)]
           [found (filter (lambda (id) (equal? origin (store:property id 'import-origin #f))) (store:buffer-list))])
      (when (> (length found) 1) (error 'create! "ambiguous promoted document" origin))
      (if (pair? found) (car found)
        ;; Authored text survives a rejected candidate, independently of its
        ;; temporary views. Create audience and origin atomically with text.
        (store:create! actor (cadr ref) (list-ref ref 4)
          (append (list (cons 'audience (list actor)) (cons 'import-origin origin))
            (remp (lambda (p) (memq (car p) '(modified modified-at conflicts publication audience import-origin disposable internal app))) (cadddr ref)))))))
  (define (configure! actor id options)
    (let ([d (view:snapshot id)])
      (let-values ([(status rows) (view:arrange! actor (list (list id (model:revision id) (view:children d) options)) '())])
        (unless (eq? status 'applied) (error 'create! "import candidate changed" status)))))
  (define (placeholder! actor window entry source reason)
    (view:create! actor source 'label 1
      (list '(catalogue . #t) (cons 'name (format "<recovered ~a>" (or (and (car entry) (caar entry)) 'output)))
        '(wrap . #t) (cons 'text (string-append "Saved presentation unavailable: " reason ". Its original data is retained in this view's recovery option."))
        (cons 'recovery entry)) '() window))
  (define (restore-app! actor window entry)
    (let ([ref (car entry)])
      (if (not (and ref (eq? (car ref) 'widget) (view:snapshot (cadr ref))))
        (placeholder! actor window entry #f "saved output or extension is not available")
        (let* ([old (cadr ref)] [d (view:snapshot old)] [wrapper? (eq? (view:kind d) 'window-tool)]
               [app (if wrapper? (let ([c (assq 'app (view:children d))]) (and c (cadr c))) old)])
          (guard (ex [else (placeholder! actor window entry old
                             (if (message-condition? ex) (condition-message ex) "copy definition is unavailable"))])
            (construction:call! actor
              (lambda (remember!)
                (unless app (error 'create! "saved tool has no app"))
                (let* ([id (remember! (view:fork! actor app
                                        (append (list (cons 'owner window))
                                          (if wrapper? (list (list 'receivers (list old window))) '()))))]
                       [options (view:options (view:snapshot id))])
                  (configure! actor id (append (list '(catalogue . #t)
                                                     (cons 'name (cond [(assq 'name (view:options d)) => cdr] [else "<recovered app>"])))
                                         (remp (lambda (o) (memq (car o) '(catalogue name))) options))) id))))))))
  (define (restore-state! actor from to)
    (let ([old (view:snapshot from)] [new (view:snapshot to)])
      (when (and old new (eq? (view:kind old) (view:kind new)) (= (view:schema old) (view:schema new))
              (equal? (view:source old) (view:source new)))
        (view:set-state! actor to (view:basis old) (view:state old))
        (for-each (lambda (c) (let ([match (assq (car c) (view:children old))])
                                (when match (restore-state! actor (cadr match) (cadr c))))) (view:children new)))))
  (define (restore-positions! actor id ref entry number)
    (let* ([d (view:snapshot id)] [source (view:source d)] [placements (caddr entry)]
           [point (cond [(assv number placements) => cdr] [(assq 'spot placements) => cdr] [else '(0 . 0)])]
           [anchor (cond [(assq 'mark placements) => cdr] [else point])]
           [top (cond [(assoc (cons 'top number) placements) => cdr] [(assq 'spot-top placements) => cdr] [else '(0 . 0)])])
      (when (eq? (view:kind d) 'editor)
        (let-values ([(lines revision changes) (store:snapshot-since source (and (eq? (car ref) 'shared) (caddr ref)))])
          (define (project p)
            (let* ([p (fold-left text:rebase-position p (if changes (map caddr changes) '()))]
                   [row (min (car p) (- (vector-length lines) 1))])
              (cons row (min (cdr p) (string-length (vector-ref lines row))))))
          (view:set-state! actor id revision (list (project point) (project anchor) (project top) (cadr entry)))))))

  (edoc "Build an unowned canonical manager from a supported screen-6 checkpoint, or return false for absent input. Validate first; promote private authored text with durable origin markers, preserve unknown payloads as inspectable placeholders, and leave the input untouched until root admission. Failed candidates release views, never the sole promoted text."
        (actor actor "named head") (owner (or model #f) "candidate lifetime") (state datum "opaque saved checkpoint") (returns (or model #f)))
  (define (create! actor owner state)
    (unless (and (list? actor) (= (length actor) 2) (eq? (car actor) 'head) (string? (cadr actor)))
      (error 'create! "expected a named head"))
    ;; A reconnect can retry while the former attachment is unwinding.
    ;; Serialize promotion lookup/allocation; no second origin index.
    (with-mutex lock (build! actor owner state)))
  (define (build! actor owner state)
    (and state
      (begin
        (valid! state)
        (let* ([digest (fingerprint state)] [entries (list->vector (list-ref state 4))]
               [documents (vector-map (lambda (entry slot)
                                        (let ([ref (car entry)])
                                          (and ref (case (car ref)
                                                     [(shared) (and (store:visible? actor (cadr ref)) (cadr ref))]
                                                     [(local) (promote! actor digest slot ref)] [else #f]))))
                            entries (list->vector (iota (vector-length entries))))])
          (construction:call! actor
            (lambda (remember!)
              (let* ([manager (remember! (manager:create! actor owner))] [leaves '()] [apps '()])
                (define (open-slot! slot window)
                  (let* ([entry (vector-ref entries slot)] [document (vector-ref documents slot)]
                         [key (cons window slot)] [old (assoc key apps)]
                         [target (or document (and old (cdr old))
                                   (let ([id (restore-app! actor window entry)]) (set! apps (cons (cons key id) apps)) id))])
                    (manager:open-document! actor manager window target)))
                (let build ([node (cadddr state)] [window (car (manager:windows actor manager))])
                  (if (eq? (car node) 'window) (set! leaves (cons (cons node window) leaves))
                    (let* ([second (manager:split! actor manager window (cadr node))]
                           [split (view:parent (view:snapshot window))])
                      (manager:resize! actor manager split (list window second) (list (caddr node) (cadddr node)))
                      (build (list-ref node 4) window) (build (list-ref node 5) second))))
                ;; Restore numbers together: intermediate duplicates would make
                ;; manager queries reject the otherwise valid candidate.
                (let-values ([(status rows)
                              (view:arrange! actor
                                (map (lambda (p) (let ([d (view:snapshot (cdr p))])
                                                   (list (cdr p) (model:revision (cdr p)) (view:children d)
                                                     (cons (cons 'number (cadar p)) (remp (lambda (o) (eq? (car o) 'number)) (view:options d)))))) leaves) '())])
                  (unless (eq? status 'applied) (error 'create! "candidate numbering changed")))
                ;; Keep hidden apps too. They remain retained under the first
                ;; window without acquiring sources until explicitly opened.
                (let ([first (cdar (reverse leaves))])
                  (do ([slot 0 (+ slot 1)]) ((= slot (vector-length entries)))
                    (when (and (car (vector-ref entries slot)) (not (vector-ref documents slot))
                            (not (exists (lambda (p) (= slot (caddar p))) leaves)))
                      (open-slot! slot first))))
                (for-each
                  (lambda (p)
                    (let* ([node (car p)] [window (cdr p)] [slot (caddr node)] [entry (vector-ref entries slot)]
                           [ref (car entry)] [document (vector-ref documents slot)])
                      (manager:set-display! actor manager window (list (cons 'wrap (list-ref node 5)) (cons 'line-numbers (list-ref node 6))))
                      (for-each (lambda (saved)
                                  (when (store:visible? actor (car saved))
                                    (restore-state! actor (cdr saved) (manager:open-document! actor manager window (car saved)))))
                        (reverse (list-ref node 7)))
                      (let ([id (open-slot! slot window)])
                        (when (and document (not (assoc document (list-ref node 7))))
                          (restore-positions! actor id ref entry (cadr node)))))) (reverse leaves))
                (manager:select! actor manager (cdr (find (lambda (p) (= (cadar p) (caddr state))) leaves)))
                (configure! actor manager (cons (cons 'import-checkpoint digest) (view:options (view:snapshot manager))))
                manager)))))))
)
