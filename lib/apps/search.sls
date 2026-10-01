;; Search commands and an independent needle-completion composition.

(import (only (foundation edoc) elibrary))
(elibrary (apps search)
  (export count (rename (search-fold-case fold-case)) (rename (search! incremental!)) init!
          preview-next! replace!)
  (import (chezscheme)
          (prefix (core region) region:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head edit) edit:) (prefix (head editor) editor:) (prefix (head editor-state) editor-state:)
          (prefix (head head) head:) (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head layout) layout:)
          (prefix (head prompt) prompt:) (prefix (head search-control) search-control:)
          (prefix (head search-host) search-host:)
          (prefix (head text-control) text-control:) (prefix (head text-source) text-source:)
          (prefix (head widget) widget:) (prefix (state model) model:) (prefix (state view) view:))

  ;; Configuration: whether the incremental search folds case the
  ;; smart way, as Emacs does -- matching ignores case only while the
  ;; needle is all lowercase; one typed capital makes it exact.
  ;; (search:fold-case #f) in config.e makes C-s always exact.
  ;; M-c inside a search toggles the current search either way.
  (edoc "Whether incremental search folds case the smart way: matching ignores case only while the needle is all lowercase."
        (value boolean))
  (define search-fold-case (make-parameter #t))

  (edoc-type needle "text to find in the current buffer, within one line; typed at M-x, its matches highlight and Tab visits them in turn"
    (predicate (lambda (v) (and (string? v) (> (string-length v) 0))))
    (within string))

  (define (get r k) (cond [(assq k r) => cdr] [else #f]))
  (define (make-preview request context commands)
    (let* ([argument (get context 'argument)] [document (get argument 'document)] [origin (get argument 'editor)])
      (if (not (and document origin)) (prompt:create-choices! request commands 'columns request)
        (let-values ([(source d) (text-control:context origin 'editor 'current)])
          (let* ([points (editor-state:points (text-control:mirror source) (text-control:revision source) d)]
                 [root (view:create! head:ui-actor request 'needle-preview 1 '()
                         (list (text-control:revision source) (if points (car points) '(0 . 0))) request)]
                 [editor (editor:create-view! head:ui-actor document '((read-only . #t)) root)]
                 [choices (prompt:create-choices! request commands 'columns root)])
            (view:arrange! head:ui-actor (list (list root 0 (list (list 'choices choices 'fit) (list 'editor editor '(grow 1))) '())) '()) root)))))
  (define (preview-service! id frame)
    (let* ([d (interaction:snapshot id)] [request (view:source d)]
           [record (model:snapshot request)] [argument (get (prompt:completion-context request) 'argument)])
      (when (and record (eq? (get (get record 'value) 'status) 'editing))
        (unless (assq 'search (view:children d))
          (let ([editor (widget:descendant id 'editor)])
            (let-values ([(source editor-d) (text-control:context editor 'editor 'current)])
              (let ([points (text-source:rebase (list (cadr (view:state d)))
                              (text-source:changes (text-control:mirror source) (car (view:state d)) (text-control:revision source)))])
                (when points (editor:move! editor (car points)))))
            (let ([search (search-control:create-preview! editor 'exact '() id)])
              (widget:arrange! (list (list id (get (model:snapshot id) 'revision)
                                       (append (view:children d) (list (list 'search search 'fit))) (view:options d)))))))
        (search-control:set-needle! (widget:descendant id 'search)
          (if (and (get argument 'literal?) (string? (get argument 'token))) (get argument 'token) "")))))

  (edoc "Visit the next or previous match in a prompt's independent needle preview. Return false for a nonliteral argument so ordinary symbol completion can proceed."
        (receiver id (view needle-preview)) (id model "needle presentation") (backwards? boolean "previous match") (returns boolean))
  (define (preview-next! id backwards?)
    (let* ([d (interaction:snapshot id)] [argument (get (prompt:completion-context (view:source d)) 'argument)])
      (and (get argument 'literal?)
        (begin (preview-service! id #f)
          (search-control:repeat! (widget:descendant id 'search) (if backwards? 'previous 'next)) #t))))

  (edoc "Start incremental search in the current editor through the ordinary event pump. C-s repeats, M-c toggles case, Return accepts and C-g returns to the safely rebased origin.")
  (define (search!) (search-host:open! (if (search-fold-case) 'smart 'exact)))

  ;;; Matching -----------------------------------------------------------------------

  (define (fold-matches basis r needle step initial)
    ;; Count and replacement proposals use the same immutable text and
    ;; selected endpoints. Rewrites retain this basis for store rebasing.
    (when (= (string-length needle) 0)
      (error 'search "empty search string"))
    (unless (equal? (cadr basis) (region:buffer r))
      (error 'search "region and text belong to different documents" r))
    (let* ([lines (car basis)]
           [m (string-length needle)]
           [start (region:start r)] [end (region:end r)])
      (for-each
        (lambda (p)
          (unless (and (< (car p) (vector-length lines))
                    (<= (cdr p) (string-length (vector-ref lines (car p)))))
            (error 'search "position is outside the document" p)))
        (list start end))
      (let rows ([row (car start)] [out initial])
        (if (> row (car end)) out
          (let* ([s (vector-ref lines row)]
                 [limit (if (= row (car end)) (cdr end) (string-length s))])
            (let hits ([at (if (= row (car start)) (cdr start) 0)] [out out])
              (let ([hit (string:search s needle at limit)])
                (if hit (hits (+ hit m) (step row hit out)) (rows (+ row 1) out)))))))))

  (edoc "How many times needle occurs in the selected region, else in the whole current buffer."
        (needle needle "the text to count, within one line")
        (returns integer) (public))
  (define (count needle)
    (fold-matches (edit:basis) (edit:current-region) needle (lambda (row col total) (+ total 1)) 0))

  (edoc "Replace every occurrence of from with to in the selected region, else in the whole current buffer: one entry of the delta log per occurrence under one batch, one undo step, point left where it was."
        (from needle "the text to find, within one line")
        (to string "its replacement")
        (returns integer "how many occurrences were replaced")
        (public) (edits))
  (define (replace! from to)
    (let* ([r (edit:current-region)] [basis (edit:basis)]
           [occurrences
            (reverse (fold-matches basis r from
                       (lambda (row col out) (cons (list (cons row col) (cons row (+ col (string-length from))) to) out)) '()))])
      (edit:call-as-one-edit!
        (format "(search:replace! ~s ~s)" from to)
        (lambda () (edit:rewrite-regions! basis occurrences)))))

  (edoc "Install search, its scoped needle-completion presentation and default C-s/M-% bindings." (public))
  (define (init!)
    (search-host:init!)
    (prompt:register-presentation! 'needle make-preview)
    (widget:register! 'needle-preview 1
      (append (remp (lambda (p) (eq? (car p) 'measure)) (layout:container 'y))
        (list (cons 'service preview-service!) (cons 'actions (list (cons 'complete preview-next!)))
          (cons 'measure (lambda (data d axis cross child)
                           (if (eq? axis 'y) '(0 6) '(0 1)))))))
    (keymap:bind-default! "C-s" search!)
    (keymap:bind-default! "M-%" (keymap:prefill replace!))))
