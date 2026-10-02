;; Guarded text commands and editing policies shared by authored controls.
(import (only (foundation edoc) elibrary))
(elibrary (head text-control)
  (export advance! basis-text call-with-intent! context current? history! lines mirror pending? register-policy! revision submit! undo-scope)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (core property) property:)
          (prefix (foundation text) text:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (head text-source) text-source:)
          (prefix (head widget) widget:)
          (prefix (state view) view:))

  (define (refuse message)
    (raise (condition (kernel:make-refusal) (make-message-condition message))))

  (define pending (make-parameter '()))
  (define policies (kernel:make-registry car))

  (edoc "Register a pure authored-text normalization policy shared by entries and editors. Given proposed line strings and logical result positions, return both normalized values. A view selects it with (policy name schema). Interactive insert/delete/paste applies it before one guarded journal edit; explicit whole-text proposals keep their supplied intent."
        (name symbol "policy name") (schema integer "positive version")
        (normalize procedure "(lines positions) returns values lines positions"))
  (define (register-policy! name schema normalize)
    (unless (and (symbol? name) (integer? schema) (exact? schema) (> schema 0) (procedure? normalize))
      (error 'register-policy! "invalid text policy"))
    (kernel:registry-add! policies (cons (list name schema) normalize)))

  (define (normalize d old span replacement positions context basis)
    (let ([profile (assq 'policy (view:options d))])
      (if (not profile) (values span replacement positions context)
        (let ([policy (kernel:registry-find policies (lambda (p) (equal? (cdr profile) (car p))))])
          (unless policy (refuse "Text editing policy is unavailable"))
          (let-values ([(proposed delta) (text:apply-edit old span replacement)])
            (let-values ([(lines points)
                          ((cdr policy) proposed
                           (map (lambda (p) (case p [(start) (text:span-start span)] [(end) (text:delta-new-end delta)] [else p])) positions))])
              (unless (and (vector? lines) (> (vector-length lines) 0)
                        (for-all (lambda (line)
                                   (and (string? line)
                                     (not (exists (lambda (c) (or (char=? c #\newline)
                                                                (and (eq? (view:kind d) 'entry) (char=? c #\return)))) (string->list line))))) (vector->list lines))
                        (or (not (eq? (view:kind d) 'entry)) (= (vector-length lines) 1))
                        (list? points) (= (length points) (length positions))
                        (for-all (lambda (p) (and (text:position? p) (< (car p) (vector-length lines))
                                               (<= (cdr p) (string-length (vector-ref lines (car p)))))) points))
                (refuse "Invalid normalized text or positions"))
              (if (equal? lines proposed) (values span replacement points context)
                (let-values ([(span replacement) (text:difference old lines)])
                  (values span replacement points
                    (append (or (property:edit-context context) '(#f "Normalize input")) (list (cons 'revision basis))))))))))))

  (edoc "The default undo scope shared by authored text controls: mine for this head's actions, or all for every actor. The command environment exposes this as edit:undo-scope."
        (value (one-of mine all)))
  (define undo-scope
    (make-parameter 'mine
      (lambda (scope)
        (unless (memq scope '(mine all)) (error 'undo-scope "expected mine or all" scope)) scope)))

  (edoc "Whether a text command is computing or admitting this view's intent. Idle anchor maintenance must wait; explicit user interactions remain allowed."
        (id model "text view") (returns boolean))
  (define (pending? id) (and (member id (pending)) #t))

  (edoc "Keep idle anchor maintenance outside a command's captured intent, including reentrant service pumps. A newer explicit interaction still supersedes its settlement."
        (id model "text view") (thunk thunk "command") (returns any))
  (define (call-with-intent! id thunk)
    (parameterize ([pending (cons id (pending))]) (thunk)))

  (edoc "Read an already acquired text mirror from a widget source envelope, without I/O."
        (source list "borrowed source snapshot") (returns any))
  (define (mirror source)
    (let ([id (and source (cdr (assq 'id source)))])
      (unless (and (pair? id) (eq? (car id) 'buffer)) (error 'text-control "expected a text buffer source" id))
      (or (text-source:lookup id) (error 'text-control "source is unavailable" id))))

  (edoc "Read a widget text snapshot's revision." (source list "source snapshot") (returns integer))
  (define (revision source) (cdr (assq 'revision source)))

  (edoc "Borrow a widget text snapshot's immutable lines." (source list "source snapshot") (returns vector))
  (define (lines source) (cdr (assq 'value source)))

  (edoc "Read an explicit text control's invocation source and descriptor; require its kind and schema without resolving a window."
        (id model "mounted text control") (kind symbol "required kind")
        (mode (list-of symbol) "current bypasses a retained action basis"))
  (define (context id kind . mode)
    (let-values ([(source d inputs) (apply widget:context id mode)])
      (unless (and d (eq? (view:kind d) kind) (= (view:schema d) 1)) (error 'text-control "unexpected text view" id kind))
      (mirror source)
      (values source d)))

  (edoc "Reconstruct the text on which this view's positions were established; missing history refuses."
        (source list "source snapshot") (d list "view descriptor") (returns vector))
  (define (basis-text source d)
    (text-source:basis-text (mirror source) (lines source) (revision source)
      (or (view:basis d) (revision source))))

  (edoc "Advance an idle text view's logical anchors through its retained delta chain. Missing history leaves the state untouched. Admission owns settlement while a command is pending; service pumps must not supersede it."
        (id model "text view") (source list "current source snapshot") (d list "current descriptor")
        (positions list "logical anchors") (state procedure "rebased positions to portable view state"))
  (define (advance! id source d positions state)
    (when (and (not (pending? id)) (view:basis d) (not (= (view:basis d) (revision source))))
      (let ([points (text-source:rebase positions (text-source:changes (mirror source) (view:basis d) (revision source)))])
        (when points (interaction:set-state! head:ui-actor id (revision source) (state points))))))

  (edoc "Whether an invocation still addresses the same source and ownership generation."
        (id model "control") (source list "source snapshot") (d list "view descriptor") (returns boolean) (effects internal))
  (define (current? id source d)
    (guard (ex [else #f])
      (let-values ([(now current inputs) (widget:context id 'current)])
        (and current (= (view:generation d) (view:generation current))
          (equal? (cdr (assq 'id source)) (cdr (assq 'id now)))))))
  (define (settle! id source d mirror revision positions state)
    ;; Accepted text survives callbacks that close/reclaim the view. Never
    ;; overwrite a newer interaction made while the store was delivering it.
    (and (current? id source d)
      (let-values ([(now current inputs) (widget:context id 'current)])
        (and (= (view:sequence d) (view:sequence current))
          (let ([points (text-source:rebase positions (text-source:changes mirror revision (text-source:revision mirror)))])
            (and (pair? points)
              (begin (interaction:set-state! head:ui-actor id (text-source:revision mirror) (state points)) #t)))))))

  (edoc "Submit stated text intent, adopt its receipt once and settle a still-current view. The state procedure maps accepted logical positions to the control's portable state."
        (id model "control") (source list "source snapshot") (d list "descriptor") (old vector "proposal text")
        (basis integer "proposal revision") (span any "replaced span") (replacement list "replacement lines")
        (context any "journal grouping and witnesses") (positions list "desired result positions") (state procedure "portable state constructor")
        (policy (list-of boolean) "apply the view's editing policy, at most one flag"))
  (define (submit! id source d old basis span replacement context positions state . policy)
    (unless (and (<= (length policy) 1) (for-all boolean? policy)) (error 'submit! "invalid policy flag"))
    (call-with-intent! id (lambda ()
                            (unless (current? id source d) (refuse "The text view was closed or its source changed"))
                            (when (cond [(assq 'read-only (view:options d)) => cdr] [else #f]) (refuse "This text view is read-only"))
                            (let-values ([(span replacement positions context)
                                          (if (and (pair? policy) (car policy)) (normalize d old span replacement positions context basis)
                                            (values span replacement positions context))])
                              (let ([m (mirror source)])
                                (let-values ([(lines rev changes points committed)
                                              (text-source:edit! head:ui-actor (list old (text-source:id m) basis) span replacement context positions)])
                                  (text-source:adopt! m basis lines rev changes)
                                  (settle! id source d m rev points state)))))) )

  (edoc "Apply source undo/redo and rebase a still-current view's logical positions through the same journal."
        (id model "control") (source list "source snapshot") (d list "descriptor")
        (direction symbol "undo or redo") (scope any "actor scope")
        (positions list "logical anchors") (state procedure "portable state constructor"))
  (define (history! id source d direction scope positions state)
    (call-with-intent! id (lambda ()
                            (unless (current? id source d) (refuse "The text view was closed or its source changed"))
                            (when (cond [(assq 'read-only (view:options d)) => cdr] [else #f]) (refuse "This text view is read-only"))
                            (let* ([m (mirror source)] [basis (or (view:basis d) (revision source))])
                              (let-values ([(status detail) (text-source:history! head:ui-actor (text-source:id m) direction scope)])
                                (when (eq? status 'applied)
                                  (text-source:open! head:ui-actor (text-source:id m) basis)
                                  (settle! id source d m basis positions state))
                                (values status detail))))) ))
