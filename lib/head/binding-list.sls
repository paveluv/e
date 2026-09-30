;; Head-local binding discovery and fitting share symbolic character marks.
(import (only (foundation edoc) elibrary))
(elibrary (head binding-list)
  (export anchor basis capture fit key-prefix? locate position)
  (import (except (chezscheme) trace) (prefix (foundation edoc) edoc:) (prefix (foundation string) string:)
    (prefix (foundation wire) wire:) (prefix (head keymap) keymap:) (prefix (head markdown-layout) markdown-layout:)
    (prefix (head mouse) mouse:) (prefix (head widget) widget:))
  (define symbolic-spans (make-weak-eq-hashtable))
  (define (spans text)
    (hashtable-ref symbolic-spans text '()))
  (define (marked text ranges)
    (unless (null? ranges)
      (hashtable-set! symbolic-spans text ranges))
    text)
  (define (join . parts)
    (let loop ([parts parts] [offset 0] [ranges '()] [out '()])
      (if (null? parts)
        (marked
          (apply string-append (reverse out))
          (apply append (reverse ranges)))
        (loop
          (cdr parts)
          (+ offset (string-length (car parts)))
          (cons
            (map (lambda (r)
                   (cons (+ offset (car r)) (+ offset (cdr r))))
                 (spans (car parts)))
            ranges)
          (cons (car parts) out)))))
  (define (action-procedure action)
    (cond
      [(procedure? action) action]
      [(keymap:call-action? action)
       (keymap:call-action-procedure action)]
      [(keymap:prefill-action? action)
       (keymap:prefill-action-procedure action)]
      [else #f]))
  (define (edits? action)
    (let* ([proc (action-procedure action)]
           [sigs (and proc (edoc:edoc-of proc))])
      (and (pair? sigs)
        (exists
          (lambda (f) (and (pair? f) (eq? (car f) 'edits)))
          (edoc:signature-flags (car sigs))))))
  (define (summary-of action)
    (let* ([proc (action-procedure action)]
           [sigs (and proc (edoc:edoc-of proc))])
      (if (pair? sigs) (edoc:signature-summary (car sigs)) "")))
  (define (trace action . substitutions)
    (map (lambda (row)
           (list
             (join
               (if (= (car row) 0)
                 ""
                 (string-append
                   (make-string (* 2 (min 8 (car row))) #\space)
                   "→ "))
               (marked (cadr row) (list-ref row 4))
               (if (cadddr row) (string-append " [" (cadddr row) "]") ""))
             (if (caddr row) (summary-of (caddr row)) "")))
      (apply keymap:action-trace action substitutions)))
  (define (shadowed? sequence nearer)
    (exists
      (lambda (context)
        (exists
          (lambda (n)
            (keymap:resolved-binding context (list-head sequence n)))
          (map (lambda (i) (+ i 1)) (iota (length sequence)))))
      nearer))
  (define (context-groups context nearer read-only? keep
            describe)
    (define (add key command description groups)
      (let ([hit (find
                   (lambda (g) (equal? (cadr g) command))
                   groups)])
        (if hit
          (map (lambda (g)
                 (if (eq? g hit) (cons (cons key (car g)) (cdr g)) g))
               groups)
          (cons (list (list key) command description) groups))))
    (let loop ([owned (keymap:context-bindings context)]
               [groups '()])
      (if (null? owned)
        (list-sort
          (lambda (a b) (string<? (car (car a)) (car (car b))))
          (map (lambda (g)
                 (cons (list-sort string<? (car g)) (cdr g)))
               groups))
        (let* ([b (cdr (car owned))]
               [action (keymap:binding-action b)]
               [command (and action
                             (if describe (describe b) (trace action)))])
          (loop
            (cdr owned)
            (if (and command
                     (not (shadowed? (keymap:binding-sequence b) nearer))
                     (not (and read-only? (edits? action)))
                     (or (not keep) (keep b)))
                (add (keymap:sequence-text (keymap:binding-sequence b))
                     command
                     (summary-of action)
                     groups)
                groups))))))
  (define (command-template procedure arguments)
    (define (formal name)
      (let ([text (string-copy (symbol->string name))])
        (marked text (list (cons 0 (string-length text))))))
    (let* ([text (keymap:action-text
                   (keymap:call (apply procedure arguments)))]
           [sigs (edoc:edoc-of procedure)]
           [sig (and sigs
                  (find
                    (lambda (s) (eq? (edoc:signature-kind s) 'procedure))
                    sigs))]
           [remaining (if sig
                        (let skip ([f (edoc:signature-formals sig)]
                                   [n (length arguments)])
                          (if (and (> n 0) (pair? f)) (skip (cdr f) (- n 1)) f))
                        'arguments)]
           [tail (let spell ([f remaining])
                   (cond
                     [(null? f) ""]
                     [(pair? f) (join " " (formal (car f)) (spell (cdr f)))]
                     [else (join " . " (formal f))]))])
      (join
        (substring text 0 (- (string-length text) 1))
        tail
        ")")))
  (define (contexts root outer key)
    (if (not root)
      outer
      (let loop ([scopes (cadr (widget:key-scopes root key))]
                 [out '()])
        (if (null? scopes)
            (append out outer)
            (let ([out (append
                         out
                         (filter (lambda (c) (not (memq c out))) (cadar scopes)))])
              (if (caddar scopes) out (loop (cdr scopes) out)))))))
  (define (reachable? root outer context binding)
    (let* ([sequence (keymap:binding-sequence binding)]
           [path (contexts root outer (car sequence))])
      (let loop ([path path] [nearer '()])
        (and (pair? path)
          (if (eq? (car path) context)
              (not (shadowed? sequence nearer))
              (loop (cdr path) (cons (car path) nearer)))))))
  (define (describe-binding root context binding)
    (let ([scope (find
                   (lambda (scope) (memq context (cadr scope)))
                   (cadr
                     (widget:key-scopes
                       root
                       (car (keymap:binding-sequence binding)))))])
      (trace
        (keymap:binding-action binding)
        (if scope (list (cons widget:target (car scope))) '()))))
  (define (action-basis action)
    (if (keymap:call-action? action)
      (cons
        (keymap:call-action-procedure action)
        (map action-basis (keymap:call-action-arguments action)))
      action))

  (edoc
    "Capture the local metadata an inspection depends on, without tracing commands or reading remote state. Compare this before rebuilding a listing."
    (root (or model #f) "explicit mounted subject")
    (outer list "outer key contexts")
    (read-only? boolean "legacy text edit policy")
    (pointer list "already resolved pointer actions")
    (returns list) (effects internal))
  (define (basis root outer read-only? pointer)
    (list root outer read-only? (+ (keymap:generation) (widget:generation))
      (and root
        (let ([scopes (widget:key-scopes root "")])
          (list (cadr scopes) (cadar scopes))))
      (map (lambda (p) (list (car p) (action-basis (cadr p))))
        pointer)
      (and root (widget:inspect root 256))))
  (define keyboard-cache (make-hashtable equal-hash equal?))

  (edoc "Whether the selected input route needs another key for this chord. Reads keymaps without changing dispatch's pending chord."
        (root (or model #f) "inspected root") (outer list "outer contexts") (sequence (list-of string) "nonempty normalized key sequence") (returns boolean) (effects internal))
  (define (key-prefix? root outer sequence)
    (let loop ([path (contexts root outer (car sequence))])
      (and (pair? path)
        (let ([prefix? (keymap:binding-prefix? (car path) sequence)])
          (if (or prefix? (keymap:resolved-binding (car path) sequence)) (and prefix? #t) (loop (cdr path)))))))
  (define (origin owned)
    (let ([owner (car owned)] [kind (keymap:binding-kind (cdr owned))])
      (cond [(eq? owner 'config) "config.e (user override)"]
        [owner (format "module ~a (~a)" owner kind)]
        [(eq? kind 'default) "built-in default"] [else "current session (user override)"])))

  (edoc
    "Produce portable inspection rows and a truncation flag from captured head facts. Trace registered forwarding only; never invoke bound commands. Work and wire output stop at the listing budget."
    (basis list "captured local metadata")
    (pointer list "actions belonging to this basis")
    (sequence (list-of list) "optional single key sequence to inspect")
    (returns list) (effects internal))
  (define (capture basis pointer . sequence)
    (unless (and (<= (length sequence) 1) (or (null? sequence) (and (pair? (car sequence)) (for-all string? (car sequence)))))
      (error 'capture "expected at most one nonempty key sequence"))
    (call/cc (lambda (done)
               (let ([root (car basis)]
                     [outer (cadr basis)]
                     [read-only? (caddr basis)]
                     [graph (list-ref basis 6)]
                     [rows '()]
                     [count 0]
                     [bytes 0]
                     [truncated? #f] [prefix-count 0] [prefix-bytes 0]
                     [cache-key (append (list-head basis 5) (list (list-ref basis 6)))])
                 (define (emit row)
                   (if (>= count 2048)
                     (set! truncated? #t)
                     (let ([size (bytevector-length (wire:encode row))])
                       (if (> (+ bytes size) 240000)
                         (set! truncated? #t)
                         (begin
                           (set! bytes (+ bytes size))
                           (set! count (+ count 1))
                           (set! rows (cons row rows)))))))
                 (define (heading key title)
                   (emit (list key title '() "" "" '())))
                 (define (group section labels steps)
                   (for-each
                     (lambda (step n)
                       (unless truncated?
                         (emit
                           (list (list section labels n) #f (if (zero? n) labels '())
                             (car step) (cadr step) (spans (car step))))))
                     steps
                     (iota (length steps))))
                 (when (pair? sequence)
                   (let* ([keys (car sequence)] [all (keymap:sequence-bindings keys)]
                          [path (contexts root outer (car keys))]
                          [hit (exists (lambda (c) (keymap:resolved-binding c keys)) path)])
                     (heading 'key (string-append "Key: " (keymap:sequence-text keys)))
                     (if hit
                       (begin
                         (heading 'resolved (format "Resolved in ~a; ~a" (keymap:binding-context (cdr hit)) (origin hit)))
                         (group 'resolved (list (keymap:sequence-text keys))
                           (if root (describe-binding root (keymap:binding-context (cdr hit)) (cdr hit)) (trace (keymap:binding-action (cdr hit))))))
                       (heading 'resolved "Resolved to: captured input, self-insert or undefined"))
                     (heading 'other-bindings "Other contextual and shadowed bindings")
                     (for-each (lambda (owned n)
                                 (unless (or truncated? (eq? owned hit))
                                   (let ([context (keymap:binding-context (cdr owned))])
                                     (group (list 'alternative n) (list (symbol->string context))
                                       (list (list (keymap:action-text (keymap:binding-action (cdr owned))) (origin owned)))))))
                       all (iota (length all)))
                     (done (list (reverse rows) truncated?))))
                 (heading 'mouse "Mouse bindings")
                 (for-each
                   (lambda (p)
                     (unless truncated?
                       (group
                         'mouse
                         (list (mouse:gesture-text (car p)))
                         (trace (cadr p)))))
                   pointer)
                 (set! prefix-count count) (set! prefix-bytes bytes)
                 (let ([cached (hashtable-ref keyboard-cache cache-key #f)])
                   (when (and cached (not truncated?) (<= (+ count (length (car cached))) 2048) (<= (+ bytes (cadr cached)) 240000))
                     (done (list (append (reverse rows) (car cached)) (and graph (caddr graph))))))
                 (let loop ([rest (contexts root outer "")] [nearer '()])
                   (unless (or (null? rest) truncated?)
                     (let* ([context (car rest)]
                            [groups (context-groups context (if root '() nearer)
                                      (and (or (not root) (eq? context 'global)) read-only?)
                                      (and root (lambda (b) (reachable? root outer context b)))
                                      (and root (lambda (b) (describe-binding root context b))))])
                       (unless (null? groups)
                         (heading
                           (list 'keys context)
                           (if (eq? context 'global)
                             "Global keys"
                             (format "~a keys" context)))
                         (for-each
                           (lambda (g)
                             (unless truncated?
                               (group (list 'keys context) (car g) (cadr g))))
                           groups))
                       (loop (cdr rest) (cons context nearer)))))
                 (when (and graph (not truncated?))
                   (heading 'commands "Widget commands")
                   (for-each
                     (lambda (r)
                       (unless truncated?
                         (let ([path (if (null? (cadr r))
                                       "root"
                                       (string:join (map symbol->string (cadr r)) "/"))])
                           (heading
                             (list 'commands (car r))
                             (format
                               "~a (~a): ~a"
                               path
                               (caddr r)
                               (edoc:type-spelling 'model (car r))))
                           (for-each
                             (lambda (c)
                               (unless truncated?
                                 (let ([proc (list-ref c 4)])
                                   (group
                                     (list 'commands (car r))
                                     (list (symbol->string (car c)))
                                     (list
                                       (list
                                         (if proc
                                           (command-template proc (cons (cadr c) (cadddr c)))
                                           (command-template
                                             (keymap:call-action-procedure (keymap:call widget:act!))
                                             (cons* (cadr c) (caddr c) (cadddr c))))
                                         (string-append
                                           (if (list-ref c 5) "" "Unavailable target. ")
                                           (if proc
                                             (summary-of proc)
                                             "Target action is not registered."))))))))
                             (cadddr r)))))
                     (widget:command-bindings root 256))
                   (heading 'composition "Composition")
                   (for-each
                     (lambda (r)
                       (unless truncated?
                         (group
                           'composition
                           (list
                             (if (null? (cadr r))
                               "root"
                               (string:join (map symbol->string (cadr r)) "/")))
                           (list
                             (list
                               (edoc:type-spelling 'model (car r))
                               (format "~a schema ~a; source ~s~a; ports ~s" (caddr r) (cadddr r)
                                 (list-ref r 4)
                                 (if (list-ref r 7) "" "; definition unavailable")
                                 (list-ref r 6)))))))
                     (car graph))
                   (unless (null? (cadr graph))
                     (heading 'connections "Typed connections")
                     (for-each
                       (lambda (e)
                         (unless truncated?
                           (group
                             'connections
                             (list (format "~s ~a" (cadr e) (caddr e)))
                             (list
                               (list
                                 (format "← ~s" (cadddr e))
                                 (format "Owner: ~s" (car e)))))))
                       (cadr graph))))
                 (let ([ordered (reverse rows)])
                   (unless truncated?
                     (when (>= (hashtable-size keyboard-cache) 8) (hashtable-clear! keyboard-cache))
                     (hashtable-set! keyboard-cache cache-key (list (list-tail ordered prefix-count) (- bytes prefix-bytes))))
                   (list ordered (or truncated? (and graph (caddr graph)))))))))
  (define (italic-runs faces)
    (let loop ([i 0] [start #f] [out '()])
      (cond [(= i (vector-length faces)) (reverse (if start (cons (cons start i) out) out))]
        [(eq? (vector-ref faces i) 'italic) (loop (+ i 1) (or start i) out)]
        [else (loop (+ i 1) #f (if start (cons (cons start i) out) out))])))

  (edoc "Fit portable inspection rows with the shared semantic document layout. Each result is (logical-anchor text italic-spans heading? character-anchors). Anchors identify a stable row, field and character, independent of wrapping."
        (rows list "portable inspection rows") (width integer "available cells") (returns vector))
  (define (fit rows width)
    (define keys (list->vector (map car rows)))
    (define (cell text roles) (list text roles '()))
    (define (row r i)
      (list i (cell (string:join (caddr r) ", ") '())
        (cell (cadddr r) (map (lambda (p) (list (car p) (cdr p) 'italic)) (list-ref r 5)))
        (cell (list-ref r 4) '())))
    (define blocks
      (let loop ([rs rows] [i 0] [table '()] [out '()])
        (define (flush) (if (null? table) out (cons (list 'table 0 #f (reverse table)) out)))
        (cond [(null? rs) (reverse (flush))]
          [(cadar rs) (loop (cdr rs) (+ i 1) '() (cons (list 'line i (cell (cadar rs) '())) (flush)))]
          [else (loop (cdr rs) (+ i 1) (cons (row (car rs) i) table) out)])))
    (let-values ([(lines faces links indices anchors) (markdown-layout:render blocks (max 1 width))])
      (list->vector
        (map (lambda (s fs i ps)
               (let ([as (vector-map (lambda (p) (cons (vector-ref keys (car p)) (cdr p))) ps)])
                 (list (vector-ref as 0) s (italic-runs fs) (and (cadr (list-ref rows i)) #t) as)))
          lines faces indices anchors))))

  (edoc "Read the logical anchor at a fitted row." (fitted vector "fitted inspection") (position integer "row") (returns any))
  (define (anchor fitted position)
    (and (> (vector-length fitted) 0) (car (vector-ref fitted (max 0 (min position (- (vector-length fitted) 1)))))))

  (edoc "Locate a logical character after reflow. Return false when the row or character is no longer present."
        (fitted vector "fitted inspection") (anchor any "stable row, field and character") (returns (or pair #f)))
  (define (position fitted anchor)
    (and (list? anchor) (= (length anchor) 3)
      (let rows ([i 0])
        (and (< i (vector-length fitted))
          (or (and (equal? (car anchor) (caar (vector-ref fitted i)))
                (let* ([ps (list-ref (vector-ref fitted i) 4)] [n (vector-length ps)])
                  (let chars ([j 0])
                    (and (< j n) (if (equal? anchor (vector-ref ps j)) (cons i j) (chars (+ j 1)))))))
            (rows (+ i 1)))))))

  (edoc "Locate a logical inspection viewport anchor. A removed row falls back to the beginning."
        (fitted vector "fitted inspection") (anchor any "logical row and character") (returns integer))
  (define (locate fitted anchor) (let ([p (position fitted anchor)]) (if p (car p) 0)))
)
